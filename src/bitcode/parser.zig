const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

const bitstream = @import("../bitstream.zig");
const codes = bitstream.codes;

const Bitcode = @import("../Bitcode.zig");

pub const Result = struct {
    arena: ArenaAllocator,
    value: union(enum) {
        success: Bitcode,
        failure: Error,
    },

    pub fn deinit(res: Result) void {
        res.arena.deinit();
    }

    pub const Error = struct {
        bit_pos: usize,
        block_stack: []Bitcode.BlockId,
        kind: Kind,

        pub const Kind = union(enum) {
            invalid_bitstream: bitstream.WalkError,
            not_bitcode_file,
            magic,
            overflow,
            end_of_stream,
            end_of_record,
            unexpected_block,
            unknown_record_code: u32,
            duplicate_block,
            module_duplicate_record: Bitcode.Module.Code,
            module_version: u8,
            type_duplicate_num_entry,
            type_no_pending_name,
            type_too_many_entries: u32, // expected count
            constants_type_not_set,
            param_attr_invalid_kind: Bitcode.Module.ParamAttrGroup.Entry.Attr.Kind,
            param_attr_invalid_key: Bitcode.Module.ParamAttrGroup.Entry.Attr.Kind.WellKnown,
            expected_nul_termination,
        };

        pub fn render(err: Error, writer: anytype) !void {
            const byte = err.bit_pos / 8;
            const bit_off = err.bit_pos % 8;
            try writer.print("bit {} (0x{x:0>4}+{}): ", .{ err.bit_pos, byte, bit_off });
            {
                try writer.print("block stack [ ", .{});
                const start = b: {
                    if (err.block_stack.len > 4) {
                        try writer.print("... ", .{});
                        break :b err.block_stack.len - 4;
                    }
                    break :b 0;
                };
                for (err.block_stack[start..]) |id| {
                    try writer.print("{} ", .{id});
                }
                try writer.print("]: ", .{});
            }
            switch (err.kind) {
                .invalid_bitstream => |we| try we.render(writer),
                .not_bitcode_file => try writer.print("too short to be a bitcode file", .{}),
                .magic => try writer.print("magic header bytes do not identify stream as bitcode", .{}),
                .overflow => try writer.print("integer overflowed while parsing", .{}),
                .end_of_stream => try writer.print("unexpected end of bitstream", .{}),
                .end_of_record => try writer.print("expected more values when parsing record", .{}),
                .unexpected_block => try writer.print("unexpected block in this context", .{}),
                .unknown_record_code => |id| try writer.print("unknown record code {}", .{id}),
                .duplicate_block => try writer.print("unexpected duplicate block", .{}),
                .module_duplicate_record => |code| try writer.print("duplicate module record {s}", .{@tagName(code)}),
                .module_version => |v| try writer.print("unsupported module version {}", .{v}),
                .type_duplicate_num_entry => try writer.print("duplicate TYPE_NUMENTRY records", .{}),
                .type_no_pending_name => try writer.print("tried to define a named struct/opaque with no name", .{}),
                .type_too_many_entries => |count| try writer.print("more TYPE entries than expected ({})", .{count}),
                .constants_type_not_set => try writer.print("type not set in CONSTANTS entry", .{}),
                .param_attr_invalid_kind => |kind| try writer.print("invalid param kind {}", .{kind}),
                .param_attr_invalid_key => |key| try writer.print("invalid well-known param attr {}", .{key}),
                .expected_nul_termination => try writer.print("expected NUL-terminated string record", .{}),
            }
        }
    };
};

/// Free the result with res.deinit().
pub fn parse(gpa: Allocator, src: []const u8) Allocator.Error!Result {
    var walker: bitstream.Walker(.{
        .last_known_block_id = @enumToInt(Bitcode.BlockId.last_known_block_id),
    }) = undefined;
    walker.init(gpa, src);
    defer walker.deinit();

    var arena = ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    if (src.len < 4) return Result{
        .arena = arena,
        .value = .{
            .failure = .{
                .bit_pos = 0,
                .block_stack = &.{},
                .kind = .not_bitcode_file,
            },
        },
    };
    const magic = walker.readMagic() catch unreachable;
    if (!std.mem.eql(u8, &magic, &Bitcode.magic)) {
        return Result{
            .arena = arena,
            .value = .{
                .failure = .{
                    .bit_pos = walker.r.pos,
                    .block_stack = &.{},
                    .kind = .magic,
                },
            },
        };
    }

    var p = parser(&walker, arena.allocator());
    p.parse() catch |err| return Result{
        .arena = arena,
        .value = .{
            .failure = .{
                .bit_pos = walker.r.pos,
                .block_stack = b: {
                    const ids = try gpa.alloc(Bitcode.BlockId, p.walker.block_stack.items.len);
                    for (p.walker.block_stack.items) |item, i| {
                        ids[i] = @intToEnum(Bitcode.BlockId, item.block_id);
                    }
                    break :b ids;
                },
                .kind = switch (err) {
                    error.EndOfStream => .end_of_stream,
                    error.InvalidBitstream => .{ .invalid_bitstream = walker.err.? },
                    error.Overflow => .overflow,
                    error.InvalidBitcode => p.err.?,
                    else => |e| return e,
                },
            },
        },
    };

    return Result{
        .arena = arena,
        .value = .{ .success = p.bc },
    };
}

fn parser(walker: anytype, allocator: Allocator) Parser(@TypeOf(walker)) {
    return Parser(@TypeOf(walker)).init(allocator, walker);
}

fn Parser(comptime Walker: type) type {
    if (@typeInfo(Walker) != .Pointer) {
        @compileError("Parser must take a *Walker, got " ++ @typeName(Walker));
    }

    return struct {
        allocator: Allocator,
        walker: Walker,
        bc: Bitcode = .{},
        found: struct {
            module: bool = false,
            identification: bool = false,
            strtab: bool = false,
            param_attr_group: bool = false,
            type_entry_count: u32 = 0,
            pending_type_name: ?[]const u8 = null,
        } = .{},
        err: ?Result.Error.Kind = null,

        const Self = @This();

        fn init(allocator: Allocator, walker: Walker) Self {
            return Self{
                .allocator = allocator,
                .walker = walker,
            };
        }

        fn parseError(self: *Self, err: Result.Error.Kind) error{InvalidBitcode}!noreturn {
            self.err = err;
            return error.InvalidBitcode;
        }

        const WalkerError = @typeInfo(Walker).Pointer.child.Error;
        const Error = error{InvalidBitcode} || WalkerError;

        fn parse(self: *Self) !void {
            while (try self.walker.next()) |item| {
                const block_id = item.enter_block; // .record is unreachable at top level
                switch (@intToEnum(Bitcode.BlockId, block_id)) {
                    .IDENTIFICATION_BLOCK_ID => try self.parseIdentificationBlock(),
                    .MODULE_BLOCK_ID => try self.parseModuleBlock(),
                    .TYPE_BLOCK_ID => try self.parseTypeBlock(),
                    .STRTAB_BLOCK_ID => try self.parseStrtabBlock(),
                    .FUNCTION_BLOCK_ID => try self.parseFunctionBlock(),
                    .SYMTAB_BLOCK_ID => try self.todoSkipBlock(.SYMTAB_BLOCK_ID),
                    else => return try self.parseError(.unexpected_block),
                }
            }
        }

        fn parseIdentificationBlock(self: *Self) Error!void {
            if (self.found.identification) {
                return try self.parseError(.duplicate_block);
            }
            self.found.identification = true;

            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return try self.parseError(.unexpected_block),
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Idendification.Code, code)) {
                    .IDENTIFICATION_CODE_STRING => {
                        self.bc.identification.string = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                    },
                    .IDENTIFICATION_CODE_EPOCH => {
                        self.bc.identification.epoch = try self.expectOp(u0);
                    },
                    _ => return try self.parseError(.{ .unknown_record_code = code }),
                },
            } else unreachable;
        }

        fn parseModuleBlock(self: *Self) Error!void {
            if (self.found.module) {
                return try self.parseError(.duplicate_block);
            }
            self.found.module = true;

            while (try self.walker.next()) |item| switch (item) {
                .enter_block => |block_id| switch (@intToEnum(Bitcode.BlockId, block_id)) {
                    .PARAMATTR_BLOCK_ID => try self.parseParamAttrBlock(&self.bc.module.param_attr),
                    .PARAMATTR_GROUP_BLOCK_ID => try self.parseParamAttrGroupBlock(),
                    .TYPE_BLOCK_ID => try self.parseTypeBlock(),
                    .VALUE_SYMTAB_BLOCK_ID => try self.parseValueSymtabBlock(),
                    .CONSTANTS_BLOCK_ID => {
                        if (self.bc.module.constants.len > 0) {
                            return try self.parseError(.duplicate_block);
                        }
                        try self.parseConstantsBlock(&self.bc.module.constants);
                    },
                    .FUNCTION_BLOCK_ID => try self.parseFunctionBlock(),
                    .METADATA_BLOCK_ID => try self.parseMetadataBlock(),
                    .METADATA_KIND_BLOCK_ID => try self.todoSkipBlock(.METADATA_KIND_BLOCK_ID),
                    .USELIST_BLOCK_ID => try self.todoSkipBlock(.USELIST_BLOCK_ID),
                    .OPERAND_BUNDLE_TAGS_BLOCK_ID => try self.todoSkipBlock(.OPERAND_BUNDLE_TAGS_BLOCK_ID),
                    .SYNC_SCOPE_NAMES_BLOCK_ID => try self.todoSkipBlock(.SYNC_SCOPE_NAMES_BLOCK_ID),
                    else => return try self.parseError(.unexpected_block),
                },
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Module.Code, code)) {
                    .MODULE_CODE_GLOBALVAR => {
                        const G = Bitcode.Module.GlobalVar;
                        var g: G = .{
                            .strtab_offset = try self.expectOp(u32),
                            .strtab_size = try self.expectOp(u32),
                            .pointer_type_index = try self.expectOp(u32),
                            .is_const = try self.expectOp(bool),
                            .init_id = try self.parseOptionalIndex(u32),
                            .linkage = try self.expectOp(G.Linkage),
                            .alignment_log2 = try self.expectOp(u16),
                            .section_index = try self.parseOptionalIndex(u32),
                        };
                        // These fields may or may not be in the record,
                        // and all have default values.
                        b: {
                            g.visibility = try self.parseOp(G.Visibility) orelse break :b;
                            g.@"threadlocal" = try self.parseOp(G.Threadlocal) orelse break :b;
                            g.unnamed_addr = try self.parseOp(G.UnnamedAddr) orelse break :b;
                            g.externally_initialized = try self.parseOp(bool) orelse break :b;
                            g.dll_storage_class = try self.parseOp(G.DllStorageClass) orelse break :b;
                            g.comdat = try self.parseOp(u64) orelse break :b; // TODO
                            g.attributes_index = try self.parseOptionalIndex(u32) orelse break :b;
                            g.preemption_specifier = try self.parseOp(G.PreemptionSpecifier) orelse break :b;
                            // undocumented:
                            // 16 partition strtab offset
                            // 17 partition strtab size
                        }
                        try self.appendOne(G, &self.bc.module.global_var, g);
                    },
                    .MODULE_CODE_FUNCTION => {
                        const F = Bitcode.Module.Function;
                        const G = Bitcode.Module.GlobalVar;
                        const f = F{
                            .strtab_offset = try self.expectOp(u32),
                            .strtab_size = try self.expectOp(u32),
                            .type_index = try self.expectOp(u32),
                            .calling_conv = try self.expectOp(F.CallingConv),
                            .is_proto = try self.expectOp(bool),
                            .linkage = try self.expectOp(G.Linkage),
                            .param_attr_index = try self.parseOptionalIndex(u32),
                            .alignment_log2 = try self.expectOp(u16),
                            .section_index = try self.parseOptionalIndex(u32),
                            .visibility = try self.expectOp(G.Visibility),
                            .gc_index = try self.parseOptionalIndex(u32),
                            .unnamed_addr = try self.expectOp(G.UnnamedAddr),
                            .prologue_data_index = try self.parseOptionalIndex(u32),
                            .dll_storage_class = try self.expectOp(G.DllStorageClass),
                            .comdat = try self.expectOp(u64), // TODO
                            .prefix_index = try self.parseOptionalIndex(u32),
                            .personality_fn_index = try self.parseOptionalIndex(u32),
                            .preemption_specifier = try self.expectOp(G.PreemptionSpecifier),
                        };
                        try self.appendOne(F, &self.bc.module.function, f);
                    },
                    .MODULE_CODE_VERSION => {
                        if (self.bc.module.version != 0) {
                            return try self.parseError(.{ .module_duplicate_record = .MODULE_CODE_VERSION });
                        }
                        const version = try self.expectOp(u8);
                        if (version != 2) {
                            // Can only parse v2 for now
                            return try self.parseError(.{ .module_version = version });
                        }
                        self.bc.module.version = @intCast(u2, version);
                    },
                    .MODULE_CODE_TRIPLE => {
                        if (self.bc.module.triple.len != 0) {
                            return try self.parseError(.{ .module_duplicate_record = .MODULE_CODE_TRIPLE });
                        }
                        self.bc.module.triple = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                    },
                    .MODULE_CODE_DATALAYOUT => {
                        if (self.bc.module.data_layout.len != 0) {
                            return try self.parseError(.{ .module_duplicate_record = .MODULE_CODE_DATALAYOUT });
                        }
                        self.bc.module.data_layout = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                    },
                    .MODULE_CODE_SOURCE_FILENAME => {
                        if (self.bc.module.source_filename.len != 0) {
                            return try self.parseError(.{ .module_duplicate_record = .MODULE_CODE_SOURCE_FILENAME });
                        }
                        self.bc.module.source_filename = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                    },
                    .MODULE_CODE_ASM => {
                        if (self.bc.module.@"asm".len != 0) {
                            return try self.parseError(.{ .module_duplicate_record = .MODULE_CODE_ASM });
                        }
                        self.bc.module.@"asm" = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                    },
                    .MODULE_CODE_SECTIONNAME => {
                        const name = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                        try self.appendOne([]const u8, &self.bc.module.section_name, name);
                    },
                    .MODULE_CODE_DEPLIB => {
                        const name = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                        try self.appendOne([]const u8, &self.bc.module.deplib, name);
                    },
                    .MODULE_CODE_ALIAS => {
                        const A = Bitcode.Module.Alias;
                        const G = Bitcode.Module.GlobalVar;
                        const a = A{
                            .strtab_offset = try self.expectOp(u32),
                            .strtab_size = try self.expectOp(u32),
                            .type_index = try self.expectOp(u32),
                            .aliasee_val_index = try self.expectOp(u32),
                            .linkage = try self.expectOp(G.Linkage),
                            .visibility = try self.expectOp(G.Visibility),
                            .dll_storage_class = try self.expectOp(G.DllStorageClass),
                            .@"threadlocal" = try self.expectOp(G.Threadlocal),
                            .unnamed_addr = try self.expectOp(G.UnnamedAddr),
                            .preemption_specifier = try self.expectOp(G.PreemptionSpecifier),
                        };
                        try self.appendOne(A, &self.bc.module.aliases, a);
                    },
                    .MODULE_CODE_GCNAME => {
                        const name = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                        try self.appendOne([]const u8, &self.bc.module.gc_name, name);
                    },
                    .MODULE_CODE_COMDAT,
                    .MODULE_CODE_VSTOFFSET,
                    .MODULE_CODE_HASH,
                    .MODULE_CODE_IFUNC,
                    => std.log.err("TODO: module code {s}", .{@tagName(@intToEnum(Bitcode.Module.Code, code))}),
                    _ => return try self.parseError(.{ .unknown_record_code = code }),
                },
            } else unreachable;
        }

        fn parseStrtabBlock(self: *Self) Error!void {
            if (self.found.strtab) {
                return try self.parseError(.duplicate_block);
            }
            self.found.strtab = true;

            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return try self.parseError(.unexpected_block),
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Strtab.Code, code)) {
                    .STRTAB_BLOB => self.bc.strtab.contents = (try self.walker.recordBlob()).?,
                    _ => return try self.parseError(.{ .unknown_record_code = code }),
                },
            } else unreachable;
        }

        fn parseTypeBlock(self: *Self) Error!void {
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return try self.parseError(.unexpected_block),
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Module.Type.Code, code)) {
                    .TYPE_CODE_NUMENTRY => {
                        if (self.bc.module.type.entries.len != 0) {
                            return try self.parseError(.type_duplicate_num_entry);
                        }
                        const count = try self.expectOp(usize);
                        self.bc.module.type.entries = try self.allocator.alloc(Bitcode.Module.Type.Entry, count);
                    },
                    .TYPE_CODE_VOID => try self.appendTypeDefinition(.void),
                    .TYPE_CODE_FLOAT => try self.appendTypeDefinition(.float),
                    .TYPE_CODE_DOUBLE => try self.appendTypeDefinition(.double),
                    .TYPE_CODE_LABEL => try self.appendTypeDefinition(.label),
                    .TYPE_CODE_OPAQUE => try self.appendTypeDefinitionNamed(.{ .@"opaque" = undefined }),
                    .TYPE_CODE_INTEGER => {
                        const width = try self.expectOp(u16);
                        try self.appendTypeDefinition(.{ .integer = width });
                    },
                    .TYPE_CODE_POINTER => {
                        const t = Bitcode.Module.Type.Entry{ .pointer = .{
                            .pointee_type_index = try self.expectOp(u32),
                            .address_space = try self.expectOp(u16),
                        } };
                        try self.appendTypeDefinition(t);
                    },
                    .TYPE_CODE_HALF => try self.appendTypeDefinition(.float),
                    .TYPE_CODE_ARRAY => {
                        try self.appendTypeDefinition(.{ .array = .{
                            .element_count = try self.expectOp(u64),
                            .element_type_index = try self.expectOp(u32),
                        } });
                    },
                    .TYPE_CODE_VECTOR => {
                        try self.appendTypeDefinition(.{ .vector = .{
                            .element_count = try self.expectOp(u64),
                            .element_type_index = try self.expectOp(u32),
                        } });
                    },
                    .TYPE_CODE_X86_FP80 => try self.appendTypeDefinition(.x86_fp80),
                    .TYPE_CODE_FP128 => try self.appendTypeDefinition(.fp128),
                    .TYPE_CODE_PPC_FP128 => try self.appendTypeDefinition(.ppc_fp128),
                    .TYPE_CODE_METADATA => try self.appendTypeDefinition(.metadata),
                    .TYPE_CODE_X86_MMX => try self.appendTypeDefinition(.x86_mmx),
                    .TYPE_CODE_STRUCT_ANON => {
                        try self.appendTypeDefinition(.{ .@"struct" = .{
                            .name = null,
                            .is_packed = try self.expectOp(bool),
                            .element_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.allocator)).?,
                        } });
                    },
                    .TYPE_CODE_STRUCT_NAME => {
                        self.found.pending_type_name = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).?;
                    },
                    .TYPE_CODE_STRUCT_NAMED => {
                        try self.appendTypeDefinitionNamed(.{ .@"struct" = .{
                            .name = undefined,
                            .is_packed = try self.expectOp(bool),
                            .element_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.allocator)).?,
                        } });
                    },
                    .TYPE_CODE_FUNCTION => {
                        try self.appendTypeDefinition(.{ .function = .{
                            .is_vararg = try self.expectOp(bool),
                            .return_type_index = try self.expectOp(u32),
                            .param_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.allocator)).?,
                        } });
                    },
                    .TYPE_CODE_BFLOAT => try self.appendTypeDefinition(.bfloat),
                    .TYPE_CODE_X86_AMX => try self.appendTypeDefinition(.x86_amx),
                    _ => return try self.parseError(.{ .unknown_record_code = code }),
                },
            } else unreachable;
        }

        fn appendTypeDefinition(self: *Self, entry: Bitcode.Module.Type.Entry) Error!void {
            if (self.found.type_entry_count == self.bc.module.type.entries.len) {
                return try self.parseError(.{ .type_too_many_entries = @intCast(u32, self.bc.module.type.entries.len) });
            }
            self.bc.module.type.entries[self.found.type_entry_count] = entry;
            self.found.type_entry_count += 1;
        }

        fn appendTypeDefinitionNamed(self: *Self, entry: Bitcode.Module.Type.Entry) Error!void {
            const name = self.found.pending_type_name orelse
                return try self.parseError(.type_no_pending_name);
            defer self.found.pending_type_name = null;

            switch (entry) {
                .@"opaque" => {
                    return try self.appendTypeDefinition(.{ .@"opaque" = .{
                        .name = name,
                    } });
                },
                .@"struct" => |s| {
                    return try self.appendTypeDefinition(.{ .@"struct" = .{
                        .name = name,
                        .is_packed = s.is_packed,
                        .element_type_indexes = s.element_type_indexes,
                    } });
                },
                else => unreachable,
            }
        }

        fn parseParamAttrBlock(self: *Self, dst: *Bitcode.Module.ParamAttr) Error!void {
            var indexes = ArrayList(u32).init(self.allocator);
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return try self.parseError(.unexpected_block),
                .end_block => break,
                .record => |code| switch (@intToEnum(Bitcode.Module.ParamAttr.Code, code)) {
                    .PARAMATTR_CODE_ENTRY => {
                        while (try self.walker.nextRecordValue(u32)) |idx| {
                            try indexes.append(idx);
                        }
                    },
                    _ => return try self.parseError(.{ .unknown_record_code = code }),
                },
            } else unreachable;
            const attr = Bitcode.Module.ParamAttr{
                .attr_group_indexes = try indexes.toOwnedSlice(),
            };
            dst.* = attr;
        }

        fn parseParamAttrGroupBlock(self: *Self) Error!void {
            if (self.found.param_attr_group) return try self.parseError(.duplicate_block);
            self.found.param_attr_group = true;
            const P = Bitcode.Module.ParamAttrGroup;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return try self.parseError(.unexpected_block),
                .end_block => break,
                .record => |code| switch (@intToEnum(P.Code, code)) {
                    .PARAMATTR_GRP_CODE_ENTRY => try self.parseParamAttrGroupEntry(),
                    _ => std.log.err("unknown param attr group code {}", .{code}),
                },
            } else unreachable;
        }

        fn parseParamAttrGroupEntry(self: *Self) Error!void {
            const P = Bitcode.Module.ParamAttrGroup;
            var entry = P.Entry{
                .group_id = try self.expectOp(u32),
                .param_idx = (try self.expectOp(P.Entry.ParamIdx.Code)).toParamIdx(),
                .attrs = undefined,
            };

            var attrs = ArrayList(P.Entry.Attr).init(self.allocator);
            while (try self.walker.nextRecordValue(u32)) |kind_code| {
                const kind = @intToEnum(P.Entry.Attr.Kind, kind_code);
                switch (kind) {
                    .well_known => {
                        const key = try self.expectOp(P.Entry.Attr.Kind.WellKnown);
                        const attr: P.Entry.Attr = .{ .well_known = switch (key) {
                            inline .alwaysinline,
                            .byval,
                            .inlinehint,
                            .inreg,
                            .minsize,
                            .naked,
                            .nest,
                            .@"noalias",
                            .nobuiltin,
                            .nocapture,
                            .nodeduplicate,
                            .noimplicitfloat,
                            .@"noinline",
                            .nonlazybind,
                            .noredzone,
                            .noreturn,
                            .nounwind,
                            .optsize,
                            .readnone,
                            .readonly,
                            .returned,
                            .returns_twice,
                            .signext,
                            .ssp,
                            .sspreq,
                            .sspstrong,
                            .sret,
                            .sanitize_address,
                            .sanitize_thread,
                            .sanitize_memory,
                            .uwtable,
                            .zeroext,
                            .builtin,
                            .cold,
                            .optnone,
                            .inalloca,
                            .nonnull,
                            .jumptable,
                            .convergent,
                            .safestack,
                            .argmemonly,
                            .swiftself,
                            .swifterror,
                            .norecurse,
                            .inaccessiblememonly,
                            .inaccessiblememonly_or_argmemonly,
                            .writeonly,
                            .speculatable,
                            .strictfp,
                            .sanitize_hwaddress,
                            .nocf_check,
                            .optforfuzzing,
                            .shadowcallstack,
                            .speculative_load_hardening,
                            .immarg,
                            .willreturn,
                            .nofree,
                            .nosync,
                            .sanitize_memtag,
                            .preallocated,
                            .no_merge,
                            .null_pointer_is_valid,
                            .noundef,
                            .byref,
                            .mustprogress,
                            .swiftasync,
                            .nosanitize_coverage,
                            .elementtype,
                            .disable_sanitizer_instrumentation,
                            .nosanitize_bounds,
                            => |val| val,
                            else => return try self.parseError(.{ .param_attr_invalid_key = key }),
                        } };
                        try attrs.append(attr);
                    },
                    .well_known_with_value => {
                        const key = try self.expectOp(P.Entry.Attr.Kind.WellKnown);
                        const attr: P.Entry.Attr = .{ .well_known = switch (key) {
                            .@"align" => .{ .@"align" = try self.expectOp(u32) },
                            .alignstack => .{ .alignstack = try self.expectOp(u32) },
                            .dereferenceable => .{ .dereferenceable = try self.expectOp(u32) },
                            .dereferenceable_or_null => .{ .dereferenceable_or_null = try self.expectOp(u32) },
                            .allocsize => b: {
                                const val = try self.expectOp(u64);
                                const elem_size = @intCast(u32, val >> 32);
                                const num_elems = @truncate(u32, val);
                                break :b .{ .allocsize = .{
                                    .elem_size = elem_size,
                                    .num_elems = if (num_elems == std.math.maxInt(u32)) null else num_elems,
                                } };
                            },
                            .vscale_range => b: {
                                const val = try self.expectOp(u64);
                                const min = @intCast(u32, val >> 32);
                                const max = @truncate(u32, val);
                                break :b .{ .vscale_range = .{
                                    .min = min,
                                    .max = if (max == std.math.maxInt(u32)) null else max,
                                } };
                            },
                            else => return try self.parseError(.{ .param_attr_invalid_key = key }),
                        } };
                        try attrs.append(attr);
                    },
                    .string => {
                        const key = try self.parseStringOpZ();
                        const attr = P.Entry.Attr{ .custom = .{ .attr = key, .value = null } };
                        try attrs.append(attr);
                    },
                    .string_with_value => {
                        const key = try self.parseStringOpZ();
                        const val = try self.parseStringOpZ();
                        const attr = P.Entry.Attr{ .custom = .{ .attr = key, .value = val } };
                        try attrs.append(attr);
                    },
                    .unknown5, .unknown6 => {
                        std.log.err("TODO: undocumented attribute encoding; skipping record remainder", .{});
                        break;
                    },
                    _ => return try self.parseError(.{ .param_attr_invalid_kind = kind }),
                }
            }
            entry.attrs = try attrs.toOwnedSlice();

            try self.appendOne(P.Entry, &self.bc.module.param_attr_group.entries, entry);
        }

        fn parseValueSymtabBlock(self: *Self) Error!void {
            try self.todoSkipBlock(.VALUE_SYMTAB_BLOCK_ID);
        }

        fn parseConstantsBlock(self: *Self, dst: *[]Bitcode.Constant) Error!void {
            var constants = ArrayList(Bitcode.Constant).init(self.allocator);
            var next_type_index: ?u32 = null;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return try self.parseError(.unexpected_block),
                .end_block => return,
                .record => |code| {
                    const rc = @intToEnum(Bitcode.Constant.Code, code);
                    switch (rc) {
                        .CST_CODE_SETTYPE => {
                            next_type_index = try self.expectOp(u32);
                            continue;
                        },
                        else => {},
                    }
                    if (next_type_index == null) {
                        return try self.parseError(.constants_type_not_set);
                    }
                    const val: Bitcode.Constant.Value = switch (rc) {
                        .CST_CODE_SETTYPE => unreachable,
                        .CST_CODE_NULL => .null,
                        .CST_CODE_UNDEF => .undef,
                        .CST_CODE_INTEGER => .{ .int = try self.expectOp(u64) },
                        .CST_CODE_WIDE_INTEGER => val: {
                            // TODO
                            break :val .{ .wide_int = (try self.walker.remainingRecordValuesAlloc(u64, self.allocator)).? };
                        },
                        .CST_CODE_FLOAT => .{ .float = try self.expectOp(u64) },
                        .CST_CODE_AGGREGATE => .{ .aggregate = (try self.walker.remainingRecordValuesAlloc(u64, self.allocator)).? },
                        .CST_CODE_STRING => .{ .string = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).? },
                        .CST_CODE_CSTRING => .{ .cstring = (try self.walker.remainingRecordValuesAlloc(u8, self.allocator)).? },
                        .CST_CODE_CE_BINOP,
                        .CST_CODE_CE_CAST,
                        .CST_CODE_CE_GEP,
                        .CST_CODE_CE_SELECT,
                        .CST_CODE_CE_EXTRACTELT,
                        .CST_CODE_CE_INSERTELT,
                        .CST_CODE_CE_SHUFFLEVEC,
                        .CST_CODE_CE_CMP,
                        .CST_CODE_INLINEASM_OLD,
                        .CST_CODE_CE_SHUFVEC_EX,
                        .CST_CODE_CE_INBOUNDS_GEP,
                        .CST_CODE_BLOCKADDRESS,
                        .CST_CODE_DATA,
                        .CST_CODE_INLINEASM_OLD2,
                        .CST_CODE_CE_GEP_WITH_INRANGE_INDEX,
                        .CST_CODE_CE_UNOP,
                        .CST_CODE_POISON,
                        .CST_CODE_DSO_LOCAL_EQUIVALENT,
                        .CST_CODE_INLINEASM_OLD3,
                        .CST_CODE_NO_CFI_VALUE,
                        .CST_CODE_INLINEASM,
                        => {
                            std.log.err("TODO: constants code {}", .{rc});
                            continue;
                        },
                        _ => return try self.parseError(.{ .unknown_record_code = code }),
                    };
                    try constants.append(.{ .type_index = next_type_index.?, .value = val });
                },
            };
            dst.* = try constants.toOwnedSlice();
        }

        fn parseFunctionBlock(self: *Self) Error!void {
            const F = Bitcode.Module.Function;
            var func: F = undefined;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => |block_id| {
                    switch (@intToEnum(Bitcode.BlockId, block_id)) {
                        .CONSTANTS_BLOCK_ID => try self.parseConstantsBlock(&func.constants),
                        .VALUE_SYMTAB_BLOCK_ID => try self.parseValueSymtabBlock(),
                        .METADATA_BLOCK_ID => try self.parseMetadataBlock(),
                        .METADATA_ATTACHMENT_ID => try self.parseMetadataAttachmentBlock(),
                        .USELIST_BLOCK_ID => try self.todoSkipBlock(.USELIST_BLOCK_ID),
                        else => return try self.parseError(.unexpected_block),
                    }
                },
                .end_block => return,
                .record => |code| {
                    std.log.err("TODO: function code {}", .{code});
                },
            };
        }

        fn parseMetadataBlock(self: *Self) Error!void {
            try self.todoSkipBlock(.METADATA_BLOCK_ID);
        }

        fn parseMetadataAttachmentBlock(self: *Self) Error!void {
            try self.todoSkipBlock(.METADATA_ATTACHMENT_ID);
        }

        fn todoSkipBlock(self: *Self, block_id: Bitcode.BlockId) Error!void {
            std.log.err("TODO: skipping block {s}", .{@tagName(block_id)});
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => |sub_id| try self.todoSkipBlock(@intToEnum(Bitcode.BlockId, sub_id)),
                .end_block => return,
                .record => {},
            } else unreachable;
        }

        fn parseStringOpZ(self: *Self) Error![]const u8 {
            var str = ArrayList(u8).init(self.allocator);
            while (try self.walker.nextRecordValue(u8)) |char| {
                if (char == 0) break;
                try str.append(char);
            } else {
                return try self.parseError(.expected_nul_termination);
            }
            return try str.toOwnedSlice();
        }

        fn parseOp(self: *Self, comptime T: type) Error!?T {
            const info = @typeInfo(T);
            switch (info) {
                .Enum => {
                    const Tag = info.Enum.tag_type;
                    const int = (try self.walker.nextRecordValue(Tag)) orelse return null;
                    return @intToEnum(T, int);
                },
                .Int => {
                    return try self.walker.nextRecordValue(T);
                },
                .Bool => {
                    return ((try self.walker.nextRecordValue(u64)) orelse return null) != 0;
                },
                .Void => {
                    _ = try self.walker.nextRecordValue(u64);
                },
                else => {
                    @compileError("cannot parse op into a " ++ @typeName(T));
                },
            }
        }

        fn expectOp(self: *Self, comptime T: type) Error!T {
            return (try self.parseOp(T)) orelse try self.parseError(.end_of_record);
        }

        fn parseOptionalIndex(self: *Self, comptime T: type) Error!?T {
            const x = (try self.parseOp(T)) orelse return null;
            return if (x == 0) null else x - 1;
        }

        fn appendOne(self: *Self, comptime T: type, slice: *[]T, val: T) Error!void {
            const len = slice.len;
            slice.* = try self.allocator.realloc(slice.*, len + 1);
            slice.*[len] = val;
        }
    };
}
