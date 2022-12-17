const std = @import("std");
const io = std.io;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const bitstream = @import("../bitstream.zig");
const codes = bitstream.codes;

pub const WalkerOptions = struct {
    /// If set, Walker pre-allocates an array of block infos for fast lookup.
    /// If set below the first valid application block ID, generates a compile error.
    last_known_block_id: ?u32 = null,
    /// If false, any block ID seen that is beyond last_known_block_id causes Walker to return an error.
    allow_unknown_block_ids: bool = false,
    /// If true, BLOCKNAME values in BLOCKINFO are stored.
    /// Doing so causes allocation for the decoded string.
    store_block_names: bool = false,
    /// If true, SETRECORDNAME values in BLOCKINFO are stored.
    /// Doing so causes allocation for the decoded string.
    store_record_names: bool = false,
};

pub const WalkError = union(enum) {
    invalid_block_id: u32,
    invalid_abbrev_id: struct {
        id: u32,
        in_block: u32,
    },
    top_level_end_block,
    top_level_define_abbrev,
    top_level_invalid_abbrev_id: u32,
    blockinfo_sub_block,
    blockinfo_no_bid,
    blockinfo_malformed_setbid,
    blockinfo_invalid_abbrev_id: u32,
    blockinfo_invalid_code: u32,
    malformed_abbrev_op,

    pub fn render(err: WalkError, writer: anytype) !void {
        switch (err) {
            .invalid_block_id => |id| try writer.print("invalid block ID {}", .{id}),
            .invalid_abbrev_id => |e| try writer.print("invalid abbrev ID {} in block {}", .{ e.id, e.in_block }),
            .top_level_end_block => try writer.print("END_BLOCK at top level", .{}),
            .top_level_define_abbrev => try writer.print("DEFINE_ABBREV at top level", .{}),
            .top_level_invalid_abbrev_id => |id| try writer.print("invalid abbrev ID {} at top level", .{id}),
            .blockinfo_sub_block => try writer.print("ENTER_SUBBLOCK in BLOCKINFO", .{}),
            .blockinfo_no_bid => try writer.print("SETBID not set in BLOCKINFO", .{}),
            .blockinfo_malformed_setbid => try writer.print("malformed SETBID in BLOCKINFO", .{}),
            .blockinfo_invalid_abbrev_id => |id| try writer.print("invalid abbrev ID {} in BLOCKINFO", .{id}),
            .blockinfo_invalid_code => |code| try writer.print("invalid record code {} in BLOCKINFO", .{code}),
            .malformed_abbrev_op => try writer.print("malformed operand in DEFINE_ABBREV", .{}),
        }
    }
};

/// Instances of Walker must not be copied.
/// Initialize like this:
///
/// ```
/// var w: bitstream.Walker(.{}) = undefined;
/// w.init();
/// ```
pub fn Walker(comptime opts: WalkerOptions) type {
    const BlockInfo = struct {
        abbrevs: std.ArrayListUnmanaged(ParsedAbbrev) = .{},
        name: Name = name_default,
        record_names: RecordNames = record_names_default,

        const Name = if (opts.store_block_names) ?[]const u8 else void;
        const name_default: Name = if (opts.store_block_names) null else {};

        const RecordNames = if (opts.store_record_names) AutoHashMapUnmanaged(u32, []const u8) else void;
        const record_names_default: RecordNames = if (opts.store_record_names) .{} else {};
    };

    const block_infos_len = if (opts.last_known_block_id) |id| b: {
        if (id < codes.block.Id.first_application_id) {
            @compileError("opts.last_known_block_id must be higher than reserved block IDs");
        }
        break :b id - codes.block.Id.first_application_id + 1;
    } else 0;

    const BlockInfoRegistry = struct {
        known: [block_infos_len]BlockInfo = [1]BlockInfo{.{}} ** block_infos_len,
        unknown: AutoHashMapUnmanaged(u32, BlockInfo) = .{},

        fn getOrCreate(self: *@This(), allocator: Allocator, id: u32) ?*BlockInfo {
            assert(id >= codes.block.Id.first_application_id);
            const index = id - codes.block.Id.first_application_id;
            if (index < self.known.len) {
                return &self.known[index];
            }
            if (!opts.allow_unknown_block_ids) return null;
            const entry = try self.unknown.getOrPutValue(allocator, id, .{});
            return entry.value_ptr;
        }
    };

    return struct {
        //! All fields are internal state and not safe to access directly.

        arena: ArenaAllocator,
        src: []const u8,
        fbs: Fbs,
        r: bitstream.Reader(Fbs.Reader),
        block_stack: ArrayListUnmanaged(StackItem) = .{},
        block_infos: BlockInfoRegistry = .{},
        state: State = .start,
        /// Set if any function returns error.InvalidBitstream
        err: ?WalkError = null,

        const Fbs = std.io.FixedBufferStream([]const u8);

        const State = union(enum) {
            start,
            block,
            unabbrev_record: struct {
                len: u32,
                index: u32,
            },
            abbrev_record: struct {
                def: *const ParsedAbbrev,
                state: union(enum) {
                    scalars_index: u32,
                    array: struct {
                        len: u32,
                        index: u32,
                    },
                    blob: u32,
                    done,
                } = .{ .scalars_index = 0 },
            },
        };

        const StackItem = struct {
            block_id: u32,
            abbrev_id_width: u5,
            local_abbrevs: ArrayListUnmanaged(ParsedAbbrev) = .{},
        };

        pub const Entry = union(enum) {
            /// Application-specific block ID.
            enter_block: u32,
            /// Exiting the current block.
            end_block,
            /// Application-specific record code.
            record: u32,
        };

        const Self = @This();

        pub const ReaderError = bitstream.Reader(Fbs.Reader).Error;
        pub const Error = ReaderError || error{ InvalidBitstream, OutOfMemory };

        /// Call on an instance of Walker (which may be undefined) to initialize.
        /// This must be called exactly once for an instance of Walker.
        /// For now, the API for initializing this type must take a pointer
        /// instead of returning a new struct instance because of
        /// interdependent pointers in the fields.
        /// https://github.com/ziglang/zig/issues/2765
        pub fn init(self: *Self, gpa: Allocator, src: []const u8) void {
            // Use literal init syntax to make sure no fields are forgotten.
            self.* = .{
                .arena = ArenaAllocator.init(gpa),
                .src = src,
                .fbs = std.io.fixedBufferStream(src),
                .r = undefined, // set below
            };
            self.r = bitstream.reader(self.fbs.reader());
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        fn walkError(self: *Self, err: WalkError) error{InvalidBitstream}!noreturn {
            self.err = err;
            return error.InvalidBitstream;
        }

        pub fn readMagic(self: *Self) ReaderError![4]u8 {
            assert(self.state == .start); // You must only call readMagic() once
            self.state = .block;
            return self.r.readMagic();
        }

        pub fn next(self: *Self) Error!?Entry {
            switch (self.state) {
                .start => unreachable, // You must call readMagic() before next()
                .block => {
                    const top = self.stackTop();
                    const width = if (top) |item| item.abbrev_id_width else 2;
                    const abbrev_id = self.r.readAbbrevId(width) catch |err| {
                        switch (err) {
                            error.EndOfStream => {
                                // EndOfStream is how we know we finished reading the stream.
                                if (top == null and self.r.pos % 32 == 0) {
                                    return null;
                                }
                                return err;
                            },
                            else => return err,
                        }
                    };
                    switch (abbrev_id) {
                        .END_BLOCK => {
                            if (top == null) {
                                return try self.walkError(.top_level_end_block);
                            }
                            _ = self.block_stack.pop();
                            try self.r.endBlock();
                            return Entry.end_block;
                        },
                        .ENTER_SUBBLOCK => {
                            const header = try self.r.readSubBlockHeader();
                            switch (header.id) {
                                .BLOCKINFO => {
                                    try self.parseBlockInfo(header.abbrev_width);
                                    return self.next(); // TODO: iterate, don't recurse
                                },
                                _ => {
                                    const block_id = @enumToInt(header.id);
                                    if (block_id < codes.block.Id.first_application_id) {
                                        return try self.walkError(.{ .invalid_block_id = block_id });
                                    }
                                    try self.block_stack.append(self.arena.allocator(), .{
                                        .block_id = block_id,
                                        .abbrev_id_width = header.abbrev_width,
                                    });
                                    return Entry{ .enter_block = block_id };
                                },
                            }
                        },
                        .DEFINE_ABBREV => {
                            if (top == null) {
                                return try self.walkError(.top_level_define_abbrev);
                            }
                            try self.parseDefineAbbrev(&top.?.local_abbrevs);
                            return self.next(); // TODO: iterate, don't recurse
                        },
                        .UNABBREV_RECORD => {
                            const code = try self.r.readVbr(u32, 6);
                            const len = try self.r.readVbr(u32, 6);
                            self.state = .{ .unabbrev_record = .{ .len = len, .index = 0 } };
                            return Entry{ .record = code };
                        },
                        _ => {
                            if (top == null) {
                                return try self.walkError(.{ .top_level_invalid_abbrev_id = @enumToInt(abbrev_id) });
                            }
                            const id = @enumToInt(abbrev_id);
                            const abbrev = try self.getAbbrev(top.?.block_id, id);
                            const code = try self.readAbbrevScalar(u32, abbrev.record_code);
                            self.state = .{ .abbrev_record = .{ .def = abbrev } };
                            return Entry{ .record = code };
                        },
                    }
                },
                .unabbrev_record, .abbrev_record => {
                    try self.skipRemainingRecordValuesAndBlob();
                    self.state = .block;
                    return self.next(); // TODO: iterate, don't recurse
                },
            }
        }

        /// Does not observe or update state.
        fn readAbbrevScalar(self: *Self, comptime T: type, encoding: ParsedAbbrev.Scalar) Error!T {
            return switch (encoding) {
                .literal => |lit| @intCast(T, lit),
                .fixed => |w| try self.r.readInt(T, w),
                .vbr => |w| try self.r.readVbr(T, w),
                .char6 => @intCast(T, try self.r.readChar6()),
            };
        }

        /// Does not observe or update state.
        fn skipAbbrevScalar(self: *Self, encoding: ParsedAbbrev.Scalar) Error!void {
            switch (encoding) {
                .literal => {},
                .fixed => |w| try self.r.skipBits(w),
                .vbr => |w| _ = try self.r.readVbr(u64, w),
                .char6 => try self.r.skipBits(6),
            }
        }

        /// If the abbreviation has a terminating blob or array,
        /// reads the length operand and updates the state.
        ///
        /// Asserts that this is only called at end of reading reading
        /// scalar operands of an abbreviated record.
        fn readAbbrevTerminatorHeaderSetState(self: *Self) Error!void {
            const abbr = self.state.abbrev_record.def;
            assert(self.state.abbrev_record.state.scalars_index == abbr.ops.len);
            if (abbr.final) |final| switch (final) {
                .array => {
                    const len = try self.r.readVbr(u32, 6);
                    self.state.abbrev_record.state = .{ .array = .{ .index = 0, .len = len } };
                },
                .blob => {
                    const len = try self.r.readVbr(u32, 6);
                    try self.r.alignToWord();
                    self.state.abbrev_record.state = .{ .blob = len };
                },
            } else {
                self.state.abbrev_record.state = .done;
            }
        }

        pub fn nextRecordValue(self: *Self, comptime T: type) Error!?T {
            switch (self.state) {
                .start, .block => unreachable, // Only call when parsing a record
                .unabbrev_record => |*rec| {
                    if (rec.index == rec.len) return null;
                    rec.index += 1;
                    return try self.r.readVbr(T, 6);
                },
                .abbrev_record => |*rec| {
                    switch (rec.state) {
                        .scalars_index => |index| {
                            if (index == rec.def.ops.len) {
                                try self.readAbbrevTerminatorHeaderSetState();
                                return try self.nextRecordValue(T);
                            }
                            rec.state.scalars_index += 1;
                            return try self.readAbbrevScalar(T, rec.def.ops[index]);
                        },
                        .array => |arr| {
                            if (arr.index == arr.len) {
                                rec.state = .done;
                                return null;
                            }
                            rec.state.array.index += 1;
                            const enc = rec.def.final.?.array;
                            return try self.readAbbrevScalar(T, enc);
                        },
                        .blob, .done => return null,
                    }
                },
            }
        }

        /// Returns null if the the record is terminated by a blob.
        /// Otherwise returns remaining elements as a slice, even if there
        /// are zero left.
        pub fn remainingRecordValuesAlloc(self: *Self, comptime T: type, allocator: Allocator) Error!?[]T {
            switch (self.state) {
                .start, .block => unreachable, // Only call when parsing a record
                .unabbrev_record => {},
                .abbrev_record => |rec| switch (rec.state) {
                    .blob, .done => return null,
                    else => {},
                },
            }
            var list = ArrayList(T).init(allocator);
            defer list.deinit();
            try self.remainingRecordValuesAllocInner(T, &list);
            return try list.toOwnedSlice();
        }

        fn remainingRecordValuesAllocInner(self: *Self, comptime T: type, list: *ArrayList(T)) Error!void {
            switch (self.state) {
                .start, .block => unreachable, // Only call when parsing a record
                .unabbrev_record => |*rec| {
                    const rem = rec.len - rec.index;
                    if (rem == 0) return;
                    try list.ensureUnusedCapacity(rem);
                    while (rec.index < rec.len) : (rec.index += 1) {
                        const val = try self.r.readVbr(T, 6);
                        list.appendAssumeCapacity(val);
                    }
                },
                .abbrev_record => |*rec| {
                    switch (rec.state) {
                        .scalars_index => |index| {
                            const rem = @intCast(u32, rec.def.ops.len - index);
                            if (rem > 0) {
                                try list.ensureUnusedCapacity(rem);
                                for (rec.def.ops[index..]) |enc| {
                                    const val = try self.readAbbrevScalar(T, enc);
                                    list.appendAssumeCapacity(val);
                                }
                                rec.state.scalars_index += rem;
                            }
                            // We also must capture any operands stored in the final array, if present.
                            try self.readAbbrevTerminatorHeaderSetState();
                            try self.remainingRecordValuesAllocInner(T, list);
                        },
                        .array => |array| {
                            const rem = @intCast(u32, array.len - array.index);
                            if (rem > 0) {
                                try list.ensureUnusedCapacity(rem);
                                var i: u32 = 0;
                                while (i < rem) : (i += 1) {
                                    const enc = rec.def.final.?.array;
                                    const val = try self.readAbbrevScalar(T, enc);
                                    list.appendAssumeCapacity(val);
                                }
                            }
                            self.state.abbrev_record.state = .done;
                        },
                        .blob => unreachable,
                        .done => {},
                    }
                },
            }
        }

        /// Returns true if a value was skipped, false if the end has been reached.
        /// Does not skip terminating blobs.
        pub fn skipNextRecordValue(self: *Self) Error!bool {
            switch (self.state) {
                .start, .block => unreachable, // Only call when parsing a record
                .unabbrev_record => |*rec| {
                    if (rec.index == rec.len) return false;
                    _ = try self.r.readVbr(u64, 6);
                    rec.index += 1;
                    return true;
                },
                .abbrev_record => |*rec| {
                    switch (rec.state) {
                        .scalars_index => |index| {
                            if (index == rec.def.ops.len) {
                                try self.readAbbrevTerminatorHeaderSetState();
                                return try self.skipNextRecordValue();
                            }
                            rec.state.scalars_index += 1;
                            const enc = rec.def.ops[index];
                            try self.skipAbbrevScalar(enc);
                            return true;
                        },
                        .array => |array| {
                            if (array.index == array.len) {
                                rec.state = .done;
                                return false;
                            }
                            rec.state.array.index += 1;
                            const enc = rec.def.final.?.array;
                            try self.skipAbbrevScalar(enc);
                            return true;
                        },
                        .blob, .done => return false,
                    }
                },
            }
        }

        pub fn skipRemainingRecordValues(self: *Self) Error!void {
            while (try self.skipNextRecordValue()) {}
        }

        pub fn skipRemainingRecordValuesAndBlob(self: *Self) Error!void {
            try self.skipRemainingRecordValues();
            _ = try self.recordBlob();
        }

        pub fn recordBlob(self: *Self) Error!?[]const u8 {
            switch (self.state) {
                .start, .block => unreachable, // Only call when parsing a record
                .unabbrev_record => return null,
                .abbrev_record => |*rec| {
                    switch (rec.state) {
                        .scalars_index => |index| {
                            if (index < rec.def.ops.len) return null;
                            try self.readAbbrevTerminatorHeaderSetState();
                            return try self.recordBlob();
                        },
                        .array => return null,
                        .blob => |len| {
                            const start: usize = self.r.pos / 8;
                            const end = start + len;
                            try self.r.skipBits(@intCast(usize, len) * 8);
                            try self.r.alignToWord();
                            rec.state = .done;
                            return self.src[start..end];
                        },
                        .done => return null,
                    }
                },
            }
        }

        fn parseBlockInfo(self: *Self, abbrev_width: u5) Error!void {
            var dst_block_id: ?u32 = null;
            while (true) {
                const id = try self.r.readAbbrevId(abbrev_width);
                switch (id) {
                    .END_BLOCK => {
                        try self.r.endBlock();
                        return;
                    },
                    .ENTER_SUBBLOCK => return try self.walkError(.blockinfo_sub_block),
                    .DEFINE_ABBREV => {
                        if (dst_block_id) |bid| {
                            const bi = try self.getBlockInfo(bid);
                            try self.parseDefineAbbrev(&bi.abbrevs);
                        } else return try self.walkError(.blockinfo_no_bid);
                    },
                    .UNABBREV_RECORD => {
                        const code = try self.r.readVbr(u32, 6);
                        const length = try self.r.readVbr(u32, 6);
                        switch (@intToEnum(codes.record.block_info.Id, code)) {
                            .BLOCKINFO_CODE_SETBID => {
                                if (length != 1) return try self.walkError(.blockinfo_malformed_setbid);
                                dst_block_id = try self.r.readVbr(u32, 6);
                            },
                            .BLOCKINFO_CODE_BLOCKNAME => @panic("TODO BLOCKNAME"),
                            .BLOCKINFO_CODE_SETRECORDNAME => @panic("TODO SETRECORDNAME"),
                            _ => return try self.walkError(.{ .blockinfo_invalid_code = code }),
                        }
                    },
                    _ => return try self.walkError(.{ .blockinfo_invalid_abbrev_id = @enumToInt(id) }),
                }
            }
        }

        fn getBlockInfo(self: *Self, block_id: u32) Error!*BlockInfo {
            return self.block_infos.getOrCreate(self.arena.allocator(), block_id) orelse
                try self.walkError(.{ .invalid_block_id = block_id });
        }

        fn stackTop(self: *Self) ?*StackItem {
            const len = self.block_stack.items.len;
            if (len == 0) return null;
            return &self.block_stack.items[len - 1];
        }

        fn readAbbrevOp(self: *Self) Error!bitstream.AbbrevOp {
            return self.r.readAbbrevOp() catch |err| switch (err) {
                error.InvalidBitstream => try self.walkError(.malformed_abbrev_op),
                else => err,
            };
        }

        fn parseDefineAbbrev(self: *Self, into: *ArrayListUnmanaged(ParsedAbbrev)) Error!void {
            const op_count = try self.r.readVbr(u32, 5);
            if (op_count == 0) return error.InvalidBitstream;

            const code_op = try self.readAbbrevOp();
            const code_enc = switch (code_op) {
                .constant, .fixed, .vbr => ParsedAbbrev.Scalar.fromAbbrevDefOp(code_op),
                .char6, .array, .blob => return error.InvalidBitstream,
            };

            const arg_count = op_count - 1;
            var args = try ArrayList(ParsedAbbrev.Scalar)
                .initCapacity(self.arena.allocator(), arg_count);
            defer args.deinit();

            var final: ?ParsedAbbrev.Aggregate = null;

            var i: u32 = 0;
            while (i < arg_count) : (i += 1) {
                const op = try self.readAbbrevOp();
                switch (op) {
                    .constant,
                    .fixed,
                    .vbr,
                    .char6,
                    => args.appendAssumeCapacity(ParsedAbbrev.Scalar.fromAbbrevDefOp(op)),
                    .array => {
                        if (i + 2 != arg_count) {
                            return error.InvalidBitstream;
                        }
                        i += 1;
                        const elem = try self.readAbbrevOp();
                        switch (elem) {
                            .constant,
                            .fixed,
                            .vbr,
                            .char6,
                            => final = .{ .array = ParsedAbbrev.Scalar.fromAbbrevDefOp(elem) },
                            else => return error.InvalidBitstream,
                        }
                    },
                    .blob => {
                        if (i + 1 != arg_count) {
                            return error.InvalidBitstream;
                        }
                        final = .blob;
                    },
                }
            }

            try into.append(self.arena.allocator(), .{
                .record_code = code_enc,
                .ops = try args.toOwnedSlice(),
                .final = final,
            });

            // TODO: Handle ambiguous abbrev ID assignment
            // Return error.InvalidBitstream if defining an abbrev when a local was already found
        }

        // Returns an error instead of null for undefined abbreviations
        // because it represents a malformed stream which cannot be decoded further.
        fn getAbbrev(self: *Self, block_id: u32, abbrev_id: u32) Error!*const ParsedAbbrev {
            assert(abbrev_id >= codes.abbrev.Id.first_application_id);
            const index = abbrev_id - codes.abbrev.Id.first_application_id;

            // Considered a "global" abbrev ID, set in a BLOCKINFO.
            const bi = try self.getBlockInfo(block_id);
            if (index < bi.abbrevs.items.len) {
                return &bi.abbrevs.items[index];
            }

            // Considered a "local" abbrev ID, set with DEFINE_ABBREV in the current block.
            const local_index = index - bi.abbrevs.items.len;
            const locals = self.block_stack.items[self.block_stack.items.len - 1].local_abbrevs;
            if (local_index < locals.items.len) {
                return &locals.items[local_index];
            }

            return try self.walkError(.{ .invalid_abbrev_id = .{ .id = abbrev_id, .in_block = block_id } });
        }
    };
}

const ParsedAbbrev = struct {
    record_code: Scalar,
    ops: []Scalar,
    final: ?Aggregate,

    const Scalar = union(enum) {
        literal: u64,
        fixed: u6,
        vbr: u5,
        char6,

        fn fromAbbrevDefOp(op: bitstream.AbbrevOp) Scalar {
            return switch (op) {
                .constant => |x| Scalar{ .literal = x },
                .fixed => |x| Scalar{ .fixed = x },
                .vbr => |x| Scalar{ .vbr = x },
                .char6 => Scalar.char6,
                .blob, .array => unreachable,
            };
        }
    };

    const Aggregate = union(enum) {
        array: Scalar,
        blob,
    };
};
