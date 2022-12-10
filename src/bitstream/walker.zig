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

        // TODO: self: *BlockInfoRegistry causes unknown identifier
        const Reg = @This();

        fn getOrCreate(self: *Reg, allocator: Allocator, id: u32) !*BlockInfo {
            assert(id >= codes.block.Id.first_application_id);
            const index = id - codes.block.Id.first_application_id;
            if (index < self.known.len) {
                return &self.known[index];
            }
            if (!opts.allow_unknown_block_ids) {
                return error.UnknownBlockId;
            }
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

        pub fn readMagic(self: *Self) ![4]u8 {
            assert(self.state == .start); // You must only call readMagic() once
            self.state = .block;
            return self.r.readMagic();
        }

        pub fn next(self: *Self) !?Entry {
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
                                return error.InvalidAbbrevId; // END_BLOCK at top level
                            }
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
                                    try self.block_stack.append(self.arena.allocator(), .{
                                        .block_id = @enumToInt(header.id),
                                        .abbrev_id_width = header.abbrev_width,
                                    });
                                    return Entry{ .enter_block = @enumToInt(header.id) };
                                },
                            }
                        },
                        .DEFINE_ABBREV => {
                            if (top == null) {
                                return error.InvalidAbbrevId; // DEFINE_ABBREV at top level
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
                                return error.InvalidAbbrevId; // Custom abbrev at top level
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

        fn parseDefineAbbrev(self: *Self, into: *ArrayListUnmanaged(ParsedAbbrev)) !void {
            _ = self;
            _ = into;
            // TODO: Handle ambiguous abbrev ID assignment
            // Return error.InvalidBitstream if defining an abbrev when a local was already found
            @panic("TODO: parse abbrev def");
        }

        fn parseBlockInfo(self: *Self, abbrev_width: u5) !void {
            _ = self;
            _ = abbrev_width;
            @panic("TODO: parse block info");
        }

        /// Does not observe or update state.
        fn readAbbrevScalar(self: *Self, comptime T: type, encoding: ParsedAbbrev.Scalar) !T {
            return switch (encoding) {
                .literal => |lit| @intCast(T, lit),
                .fixed => |w| try self.r.readInt(T, w),
                .vbr => |w| try self.r.readVbr(T, w),
                .char6 => @intCast(T, try self.r.readChar6()),
            };
        }

        /// Does not observe or update state.
        fn skipAbbrevScalar(self: *Self, encoding: ParsedAbbrev.Scalar) !void {
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
        fn readAbbrevTerminatorHeaderSetState(self: *Self) !void {
            const abbr = self.state.abbrev_record.def;
            assert(self.state.abbrev_record.state.scalars_index == abbr.ops.len);
            if (abbr.final) |final| switch (final) {
                .array => |enc| {
                    const len = try self.readAbbrevScalar(u32, enc);
                    self.state.abbrev_record.state = .{ .array = .{ .index = 0, .len = len } };
                },
                .blob => {
                    const len = try self.r.readVbr(u32, 6);
                    try self.r.alignToWord();
                    self.state.abbrev_record.state = .{ .blob = len };
                },
            };
            self.state.abbrev_record.state = .done;
        }

        pub fn nextRecordValue(self: *Self, comptime T: type) !?T {
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

        pub fn remainingRecordValuesAlloc(self: *Self, comptime T: type, allocator: Allocator) !?[]T {
            var list = ArrayList(T).init(allocator);
            errdefer list.deinit();
            try self.remainingRecordValuesAllocInner(T, &list);
            if (list.items.len > 0) {
                return list.toOwnedSlice();
            }
            list.deinit();
            return null;
        }

        fn remainingRecordValuesAllocInner(self: *Self, comptime T: type, list: *ArrayList(T)) !void {
            switch (self.state) {
                .start, .block => unreachable, // Only call when parsing a record
                .unabbrev_record => |*rec| {
                    const rem = rec.len - rec.index;
                    if (rem == 0) return;
                    try list.ensureUnusedCapacity(rem);
                    while (rec.index < rec.len) : (rec.index += 1) {
                        const val = try self.r.readVbr(T, 6);
                        list.appendAssumeCapacity(T, val);
                    }
                },
                .abbrev_record => |*rec| {
                    switch (rec.state) {
                        .scalars_index => |index| {
                            const rem = rec.def.ops.len - index;
                            if (rem > 0) {
                                try list.ensureUnusedCapacity(rem);
                                for (rec.def.ops[index..]) |enc| {
                                    const val = try self.readAbbrevScalar(enc);
                                    list.appendAssumeCapacity(val);
                                }
                                rec.state.scalars_index += rem;
                            }
                            // We also must capture any operands stored in the final array, if present.
                            try self.readAbbrevTerminatorHeaderSetState();
                            try self.remainingRecordValuesAllocInner(T, list);
                        },
                        .array => |array| {
                            const rem = array.len - array.index;
                            if (rem > 0) {
                                try list.ensureUnusedCapacity(rem);
                                var i: u32 = 0;
                                while (i < rem) : (i += 1) {
                                    const enc = rec.def.final.?.array;
                                    const val = try self.readAbbrevScalar(enc);
                                    list.appendAssumeCapacity(T, val);
                                }
                            }
                            self.state.abbrev_record.state = .done;
                        },
                        .blob, .done => {},
                    }
                },
            }
        }

        /// Returns true if a value was skipped, false if the end has been reached.
        /// Does not skip terminating blobs.
        pub fn skipNextRecordValue(self: *Self) !bool {
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
                            const enc = rec.def.final.?.array;
                            try self.skipAbbrevScalar(enc);
                            return true;
                        },
                        .blob, .done => return false,
                    }
                },
            }
        }

        pub fn skipRemainingRecordValues(self: *Self) !void {
            while (try self.skipNextRecordValue()) {}
        }

        pub fn skipRemainingRecordValuesAndBlob(self: *Self) !void {
            try self.skipRemainingRecordValues();
            _ = try self.recordBlob();
        }

        pub fn recordBlob(self: *Self) !?[]const u8 {
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
                            rec.state = .done;
                            const start: usize = self.r.pos / 8;
                            const end = start + len;
                            return self.src[start..end];
                        },
                        .done => return null,
                    }
                },
            }
        }

        fn getBlockInfo(self: *Self, id: u32) !*BlockInfo {
            return self.block_infos.getOrCreate(self.arena.allocator(), id);
        }

        fn stackTop(self: *Self) ?*StackItem {
            const len = self.block_stack.items.len;
            if (len == 0) return null;
            return &self.block_stack.items[len - 1];
        }

        // Returns an error instead of null for undefined abbreviations
        // because it represents a malformed stream which cannot be decoded further.
        fn getAbbrev(self: *Self, block_id: u32, abbrev_id: u32) !*const ParsedAbbrev {
            assert(abbrev_id >= codes.abbrev.Id.first_application_id);
            const index = codes.abbrev.Id.first_application_id - abbrev_id;

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

            return error.InvalidAbbrevId;
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
    };

    const Aggregate = union(enum) {
        array: Scalar,
        blob,
    };
};
