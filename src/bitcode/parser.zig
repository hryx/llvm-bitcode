const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

const bitstream = @import("../bitstream.zig");
const codes = bitstream.codes;

const Bitcode = @import("../Bitcode.zig");

pub const Result = union(enum) {
    success: Bitcode,
    failure: []ParseError,
};

pub const ParseError = struct {
    pos: usize,
    err: union(enum) {
        unknown_block_id: u32,
        unknown_record_code: struct {
            code: u32,
            in_block: Bitcode.BlockId,
        },
        duplicate_block: Bitcode.BlockId,
    },

    pub fn format(e: ParseError) void {
        _ = e;
        @panic("TODO");
    }
};

pub fn parse(gpa: Allocator, src: []const u8) !Result {
    var walker: bitstream.Walker(.{
        .last_known_block_id = @enumToInt(Bitcode.BlockId.last_known_block_id),
    }) = undefined;
    walker.init(gpa, src);
    defer walker.deinit();

    const magic = try walker.readMagic();
    if (!std.mem.eql(u8, &magic, &Bitcode.magic)) {
        return error.NotBitcode; // TODO
    }

    var p = parser(&walker, gpa);
    errdefer p.deinit();
    p.parse() catch |err| switch (err) {
        // TODO: append error and return Result
        // error.EndOfStream => {},
        // error.EndOfRecord => {},
        // error.Overflow => {},
        else => return err,
    };

    return Result{ .success = p.bc };
}

fn parser(walker: anytype, gpa: Allocator) Parser(@TypeOf(walker)) {
    return Parser(@TypeOf(walker)).init(gpa, walker);
}

fn Parser(comptime Walker: type) type {
    if (@typeInfo(Walker) != .Pointer) {
        @compileError("Parser must take a *Walker, got " ++ @typeName(Walker));
    }

    return struct {
        arena: ArenaAllocator,
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

        const Self = @This();

        fn init(gpa: Allocator, walker: Walker) Self {
            return Self{
                .arena = ArenaAllocator.init(gpa),
                .walker = walker,
            };
        }

        fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        fn parse(self: *Self) !void {
            while (try self.walker.next()) |item| {
                const block_id = item.enter_block; // .record is unreachable at top level
                switch (@intToEnum(Bitcode.BlockId, block_id)) {
                    .IDENTIFICATION_BLOCK_ID => try self.parseIdentificationBlock(),
                    .MODULE_BLOCK_ID => try self.parseModuleBlock(),
                    .TYPE_BLOCK_ID => try self.parseTypeBlock(),
                    .STRTAB_BLOCK_ID => try self.parseStrtabBlock(),
                    .MODULE_STRTAB_BLOCK_ID,
                    .PARAMATTR_BLOCK_ID,
                    .PARAMATTR_GROUP_BLOCK_ID,
                    .CONSTANTS_BLOCK_ID,
                    .FUNCTION_BLOCK_ID,
                    .VALUE_SYMTAB_BLOCK_ID,
                    .METADATA_BLOCK_ID,
                    .METADATA_ATTACHMENT_ID,
                    .USELIST_BLOCK_ID,
                    .GLOBALVAL_SUMMARY_BLOCK_ID,
                    .OPERAND_BUNDLE_TAGS_BLOCK_ID,
                    .METADATA_KIND_BLOCK_ID,
                    .FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID,
                    .SYMTAB_BLOCK_ID,
                    .SYNC_SCOPE_NAMES_BLOCK_ID,
                    => return error.TODO,
                    _ => return error.InvalidBitcode,
                }
            }
        }

        fn parseIdentificationBlock(self: *Self) !void {
            if (self.found.identification) return error.InvalidBitcode;
            self.found.identification = true;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return error.InvalidBitcode,
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Idendification.Code, code)) {
                    .IDENTIFICATION_CODE_STRING => {
                        self.bc.identification.string = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                    },
                    .IDENTIFICATION_CODE_EPOCH => {
                        self.bc.identification.epoch = try self.parseOp(u0);
                    },
                    _ => std.log.warn("unknown identification record code {}", .{code}),
                },
            } else unreachable;
        }

        fn parseModuleBlock(self: *Self) !void {
            if (self.found.module) return error.InvalidBitcode;
            self.found.module = true;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => |block_id| switch (@intToEnum(Bitcode.BlockId, block_id)) {
                    .PARAMATTR_BLOCK_ID => try self.parseParamAttrBlock(),
                    .PARAMATTR_GROUP_BLOCK_ID => try self.parseParamAttrGroupBlock(),
                    .TYPE_BLOCK_ID => try self.parseTypeBlock(),
                    .VALUE_SYMTAB_BLOCK_ID => try self.parseValueSymtabBlock(),
                    .CONSTANTS_BLOCK_ID => {
                        if (self.bc.module.constants.len > 0) return error.InvalidBitcode;
                        try self.parseConstantsBlock(&self.bc.module.constants);
                    },
                    .FUNCTION_BLOCK_ID => try self.parseFunctionBlock(),
                    .METADATA_BLOCK_ID => try self.parseMetadataBlock(),
                    .METADATA_ATTACHMENT_ID => try self.parseMetadataAttachmentBlock(),
                    else => return error.InvalidBitcode,
                },
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Module.Code, code)) {
                    .MODULE_CODE_GLOBALVAR => {
                        const G = Bitcode.Module.GlobalVar;
                        const g = G{
                            .strtab_offset = try self.parseOp(u32),
                            .strtab_size = try self.parseOp(u32),
                            .pointer_type_index = try self.parseOp(u32),
                            .is_const = try self.parseOp(bool),
                            .init_id = try self.parseOptionalIndex(u32),
                            .linkage = try self.parseOp(G.Linkage),
                            .alignment_log2 = try self.parseOp(u16),
                            .section_index = try self.parseOptionalIndex(u32),
                            .visibility = try self.parseOp(G.Visibility),
                            .@"threadlocal" = try self.parseOp(G.Threadlocal),
                            .unnamed_addr = try self.parseOp(G.UnnamedAddr),
                            .externally_initialized = try self.parseOp(bool),
                            .dll_storage_class = try self.parseOp(G.DllStorageClass),
                            .comdat = try self.parseOp(u64), // TODO
                            .attributes_index = try self.parseOptionalIndex(u32),
                            .preemption_specifier = try self.parseOp(G.PreemptionSpecifier),
                            // undocumented:
                            // 16 partition strtab offset
                            // 17 partition strtab size
                        };
                        try self.appendOne(G, &self.bc.module.global_var, g);
                    },
                    .MODULE_CODE_FUNCTION => {
                        const F = Bitcode.Module.Function;
                        const G = Bitcode.Module.GlobalVar;
                        const f = F{
                            .strtab_offset = try self.parseOp(u32),
                            .strtab_size = try self.parseOp(u32),
                            .type_index = try self.parseOp(u32),
                            .calling_conv = try self.parseOp(F.CallingConv),
                            .is_proto = try self.parseOp(bool),
                            .linkage = try self.parseOp(G.Linkage),
                            .param_attr_index = try self.parseOptionalIndex(u32),
                            .alignment_log2 = try self.parseOp(u16),
                            .section_index = try self.parseOptionalIndex(u32),
                            .visibility = try self.parseOp(G.Visibility),
                            .gc_index = try self.parseOptionalIndex(u32),
                            .unnamed_addr = try self.parseOp(G.UnnamedAddr),
                            .prologue_data_index = try self.parseOptionalIndex(u32),
                            .dll_storage_class = try self.parseOp(G.DllStorageClass),
                            .comdat = try self.parseOp(u64), // TODO
                            .prefix_index = try self.parseOptionalIndex(u32),
                            .personality_fn_index = try self.parseOptionalIndex(u32),
                            .preemption_specifier = try self.parseOp(G.PreemptionSpecifier),
                        };
                        try self.appendOne(F, &self.bc.module.function, f);
                    },
                    .MODULE_CODE_VERSION => {
                        if (self.bc.module.version != 0) {
                            return error.InvalidBitcode;
                        }
                        const version = try self.parseOp(u2);
                        if (version != 2) {
                            return error.InvalidBitcode; // Can only parse v2 for now
                        }
                        self.bc.module.version = version;
                    },
                    .MODULE_CODE_TRIPLE => {
                        if (self.bc.module.triple.len != 0) {
                            return error.InvalidBitcode;
                        }
                        self.bc.module.triple = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                    },
                    .MODULE_CODE_DATALAYOUT => {
                        if (self.bc.module.data_layout.len != 0) {
                            return error.InvalidBitcode;
                        }
                        self.bc.module.data_layout = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                    },
                    .MODULE_CODE_SOURCE_FILENAME => {
                        if (self.bc.module.source_filename.len != 0) {
                            return error.InvalidBitcode;
                        }
                        self.bc.module.source_filename = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                    },
                    .MODULE_CODE_ASM => {
                        if (self.bc.module.@"asm".len != 0) {
                            return error.InvalidBitcode;
                        }
                        self.bc.module.@"asm" = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                    },
                    .MODULE_CODE_SECTIONNAME => {
                        const name = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                        try self.appendOne([]const u8, &self.bc.module.section_name, name);
                    },
                    .MODULE_CODE_DEPLIB => {
                        const name = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                        try self.appendOne([]const u8, &self.bc.module.deplib, name);
                    },
                    .MODULE_CODE_ALIAS => {
                        const A = Bitcode.Module.Alias;
                        const G = Bitcode.Module.GlobalVar;
                        const a = A{
                            .strtab_offset = try self.parseOp(u32),
                            .strtab_size = try self.parseOp(u32),
                            .type_index = try self.parseOp(u32),
                            .aliasee_val_index = try self.parseOp(u32),
                            .linkage = try self.parseOp(G.Linkage),
                            .visibility = try self.parseOp(G.Visibility),
                            .dll_storage_class = try self.parseOp(G.DllStorageClass),
                            .@"threadlocal" = try self.parseOp(G.Threadlocal),
                            .unnamed_addr = try self.parseOp(G.UnnamedAddr),
                            .preemption_specifier = try self.parseOp(G.PreemptionSpecifier),
                        };
                        try self.appendOne(A, &self.bc.module.aliases, a);
                    },
                    .MODULE_CODE_GCNAME => {
                        const name = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                        try self.appendOne([]const u8, &self.bc.module.gc_name, name);
                    },
                    _ => std.log.warn("unknown module record code {}", .{code}),
                },
            } else unreachable;
        }

        fn parseStrtabBlock(self: *Self) !void {
            if (self.found.strtab) return error.InvalidBitcode;
            self.found.strtab = true;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return error.InvalidBitcode,
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Strtab.Code, code)) {
                    .STRTAB_BLOB => self.bc.strtab.contents = (try self.walker.recordBlob()).?,
                    _ => std.log.warn("unknown strtab code {}", .{code}),
                },
            } else unreachable;
        }

        fn parseTypeBlock(self: *Self) !void {
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => return error.InvalidBitcode,
                .end_block => return,
                .record => |code| switch (@intToEnum(Bitcode.Module.Type.Code, code)) {
                    .TYPE_CODE_NUMENTRY => {
                        if (self.bc.module.type.entries.len != 0) {
                            return error.InvalidBitcode;
                        }
                        const count = try self.parseOp(usize);
                        self.bc.module.type.entries = try self.arena.allocator().alloc(Bitcode.Module.Type.Entry, count);
                    },
                    .TYPE_CODE_VOID => try self.appendTypeDefinition(.void),
                    .TYPE_CODE_FLOAT => try self.appendTypeDefinition(.float),
                    .TYPE_CODE_DOUBLE => try self.appendTypeDefinition(.double),
                    .TYPE_CODE_LABEL => try self.appendTypeDefinition(.label),
                    .TYPE_CODE_OPAQUE => try self.appendTypeDefinitionNamed(.{ .@"opaque" = undefined }),
                    .TYPE_CODE_INTEGER => {
                        const width = try self.parseOp(u16);
                        try self.appendTypeDefinition(.{ .integer = width });
                    },
                    .TYPE_CODE_POINTER => {
                        const t = Bitcode.Module.Type.Entry{ .pointer = .{
                            .pointee_type_index = try self.parseOp(u32),
                            .address_space = try self.parseOp(u16),
                        } };
                        try self.appendTypeDefinition(t);
                    },
                    .TYPE_CODE_HALF => try self.appendTypeDefinition(.float),
                    .TYPE_CODE_ARRAY => {
                        try self.appendTypeDefinition(.{ .array = .{
                            .element_count = try self.parseOp(u64),
                            .element_type_index = try self.parseOp(u32),
                        } });
                    },
                    .TYPE_CODE_VECTOR => {
                        try self.appendTypeDefinition(.{ .vector = .{
                            .element_count = try self.parseOp(u64),
                            .element_type_index = try self.parseOp(u32),
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
                            .is_packed = try self.parseOp(bool),
                            .element_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.arena.allocator())).?,
                        } });
                    },
                    .TYPE_CODE_STRUCT_NAME => {
                        self.found.pending_type_name = (try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator())).?;
                    },
                    .TYPE_CODE_STRUCT_NAMED => {
                        try self.appendTypeDefinitionNamed(.{ .@"struct" = .{
                            .name = undefined,
                            .is_packed = try self.parseOp(bool),
                            .element_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.arena.allocator())).?,
                        } });
                    },
                    .TYPE_CODE_FUNCTION => {
                        try self.appendTypeDefinition(.{ .function = .{
                            .is_vararg = try self.parseOp(bool),
                            .return_type_index = try self.parseOp(u32),
                            .param_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.arena.allocator())).?,
                        } });
                    },
                    .TYPE_CODE_BFLOAT => try self.appendTypeDefinition(.bfloat),
                    .TYPE_CODE_X86_AMX => try self.appendTypeDefinition(.x86_amx),
                    _ => std.log.warn("unknown type code {}", .{code}),
                },
            } else unreachable;
        }

        fn appendTypeDefinition(self: *Self, entry: Bitcode.Module.Type.Entry) !void {
            if (self.found.type_entry_count == self.bc.module.type.entries.len) {
                std.log.err("found too many type entries according to NUMENTRY", .{});
                return error.InvalidBitcode;
            }
            self.bc.module.type.entries[self.found.type_entry_count] = entry;
            self.found.type_entry_count += 1;
        }

        fn appendTypeDefinitionNamed(self: *Self, entry: Bitcode.Module.Type.Entry) !void {
            const name = self.found.pending_type_name orelse return error.InvalidBitcode;
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

        fn parseParamAttrBlock(self: *Self) !void {
            _ = self;
            return error.TODO;
        }

        fn parseParamAttrGroupBlock(self: *Self) !void {
            if (self.found.param_attr_group) return error.InvalidBitcode;
            self.found.param_attr_group = true;
            const P = Bitcode.Module.ParamAttrGroup;
            while (try self.walker.next()) |item| switch (item) {
                .enter_block => {},
                .end_block => break,
                .record => |code| switch (@intToEnum(P.Code, code)) {
                    .PARAMATTR_GRP_CODE_ENTRY => try self.parseParamAttrGroupEntry(),
                    _ => std.log.warn("unknown param attr group code {}", .{code}),
                },
            } else unreachable;
        }

        fn parseParamAttrGroupEntry(self: *Self) !void {
            const P = Bitcode.Module.ParamAttrGroup;
            var entry = P.Entry{
                .group_id = try self.parseOp(u32),
                .param_idx = (try self.parseOp(P.Entry.ParamIdx.Code)).toParamIdx(),
                .attrs = undefined,
            };

            var attrs = ArrayList(P.Entry.Attr).init(self.arena.allocator());
            while (try self.walker.nextRecordValue(u32)) |kind_code| {
                const kind = @intToEnum(P.Entry.Attr.Kind, kind_code);
                switch (kind) {
                    .well_known => {
                        const key = try self.parseOp(P.Entry.Attr.Kind.WellKnown);
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
                            else => return error.InvalidBitcode,
                        } };
                        try attrs.append(attr);
                    },
                    .well_known_with_value => {
                        const key = try self.parseOp(P.Entry.Attr.Kind.WellKnown);
                        const attr: P.Entry.Attr = .{ .well_known = switch (key) {
                            .@"align" => .{ .@"align" = try self.parseOp(u32) },
                            .alignstack => .{ .alignstack = try self.parseOp(u32) },
                            .dereferenceable => .{ .dereferenceable = try self.parseOp(u32) },
                            .dereferenceable_or_null => .{ .dereferenceable_or_null = try self.parseOp(u32) },
                            .allocsize => b: {
                                const val = try self.parseOp(u64);
                                const elem_size = @intCast(u32, val >> 32);
                                const num_elems = @truncate(u32, val);
                                break :b .{ .allocsize = .{
                                    .elem_size = elem_size,
                                    .num_elems = if (num_elems == std.math.maxInt(u32)) null else num_elems,
                                } };
                            },
                            .vscale_range => b: {
                                const val = try self.parseOp(u64);
                                const min = @intCast(u32, val >> 32);
                                const max = @truncate(u32, val);
                                break :b .{ .vscale_range = .{
                                    .min = min,
                                    .max = if (max == std.math.maxInt(u32)) null else max,
                                } };
                            },
                            else => return error.InvalidBitcode,
                        } };
                        try attrs.append(attr);
                    },
                    .string => {
                        const key = try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator()) orelse return error.InvalidBitcode;
                        const attr = P.Entry.Attr{ .custom = .{ .attr = key, .value = null } };
                        try attrs.append(attr);
                    },
                    .string_with_value => {
                        const kv = try self.walker.remainingRecordValuesAlloc(u8, self.arena.allocator()) orelse return error.InvalidBitcode;
                        const key_end = std.mem.indexOfScalar(u8, kv, 0) orelse return error.InvalidBitcode;
                        if (key_end + 2 > kv.len) return error.InvalidBitcode;
                        const key = kv[0..key_end];
                        const val_end = (std.mem.indexOfScalar(u8, kv[key_end + 1 ..], 0) orelse return error.InvalidBitcode) + key_end;
                        const val = kv[key_end + 1 .. val_end];
                        const attr = P.Entry.Attr{ .custom = .{ .attr = key, .value = val } };
                        try attrs.append(attr);
                    },
                    _ => return error.InvalidBitcode,
                }
            }
            entry.attrs = try attrs.toOwnedSlice();

            try self.appendOne(P.Entry, &self.bc.module.param_attr_group.entries, entry);
        }

        fn parseValueSymtabBlock(self: *Self) !void {
            _ = self;
            return error.TODO;
        }

        fn parseConstantsBlock(self: *Self, constants: *[]Bitcode.Constant) !void {
            _ = self;
            _ = constants;
            return error.TODO;
        }

        fn parseFunctionBlock(self: *Self) !void {
            _ = self;
            return error.TODO;
        }

        fn parseMetadataBlock(self: *Self) !void {
            _ = self;
            return error.TODO;
        }

        fn parseMetadataAttachmentBlock(self: *Self) !void {
            _ = self;
            return error.TODO;
        }

        fn parseOp(self: *Self, comptime T: type) !T {
            const info = @typeInfo(T);
            switch (info) {
                .Enum => {
                    const Tag = info.Enum.tag_type;
                    const int = try self.mustReadOp(Tag);
                    return @intToEnum(T, int);
                },
                .Int => {
                    return try self.mustReadOp(T);
                },
                .Bool => {
                    return (try self.mustReadOp(u64) != 0);
                },
                .Void => {
                    _ = try self.mustReadOp(u64);
                },
                else => {
                    @compileError("cannot parse op into a " ++ @typeName(T));
                },
            }
        }

        fn mustReadOp(self: *Self, comptime T: type) !T {
            return (try self.walker.nextRecordValue(T)) orelse error.EndOfRecord;
        }

        fn parseOptionalIndex(self: *Self, comptime T: type) !?T {
            const x = try self.parseOp(T);
            return if (x == 0) null else x - 1;
        }

        fn appendOne(self: *Self, comptime T: type, slice: *[]T, val: T) !void {
            const len = slice.len;
            slice.* = try self.arena.allocator().realloc(slice.*, len + 1);
            slice.*[len] = val;
        }
    };
}
