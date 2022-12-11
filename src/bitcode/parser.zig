const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

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
        gpa: Allocator,
        walker: Walker,
        bc: Bitcode = .{},
        found: struct {
            module: bool = false,
            identification: bool = false,
            strtab: bool = false,
            type_entry_count: u32 = 0,
            pending_type_name: ?[]const u8 = null,
        } = .{},

        const Self = @This();

        fn init(gpa: Allocator, walker: Walker) Self {
            return Self{
                .gpa = gpa,
                .walker = walker,
            };
        }

        fn parse(self: *Self) !void {
            while (try self.walker.next()) |item| {
                const block_id = item.enter_block; // .record is unreachable at top level
                try self.parseBlock(@intToEnum(Bitcode.BlockId, block_id));
            }
        }

        fn ParseRecordFn(comptime RecordCode: type) type {
            return fn (*Self, RecordCode) anyerror!void;
        }

        const BlockLoopFn = fn (*Self) anyerror!void;

        fn blockLoop(comptime RecordCode: type, comptime recFn: ParseRecordFn(RecordCode)) BlockLoopFn {
            return (struct {
                fn doBlock(self: *Self) !void {
                    while (try self.walker.next()) |item| switch (item) {
                        .enter_block => |sub_id| try self.parseBlock(@intToEnum(Bitcode.BlockId, sub_id)),
                        .end_block => break,
                        .record => |code| try recFn(self, @intToEnum(RecordCode, code)),
                    } else unreachable; // walker ensures stream ends with END_BLOCK
                }
            }).doBlock;
        }

        fn parseBlock(self: *Self, block_id: Bitcode.BlockId) anyerror!void {
            switch (block_id) {
                .MODULE_BLOCK_ID => {
                    if (self.found.module) {
                        std.log.err("duplicate module block", .{});
                        return error.InvalidBitcode;
                    }
                    self.found.module = true;
                    try blockLoop(Bitcode.Module.Code, parseModuleRecord)(self);
                },
                .IDENTIFICATION_BLOCK_ID => {
                    if (self.found.identification) {
                        std.log.err("duplicate identification block", .{});
                        return error.InvalidBitcode;
                    }
                    self.found.identification = true;
                    try blockLoop(Bitcode.Idendification.Code, parseIdentificationRecord)(self);
                },
                .TYPE_BLOCK_ID => try blockLoop(Bitcode.Module.Type.Code, parseTypeRecord)(self),
                .STRTAB_BLOCK_ID => try blockLoop(Bitcode.Strtab.Code, parseStrtabRecord)(self),
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
                => {
                    std.log.err("TODO: parse block ID {}", .{block_id});
                    try blockLoop(DummyCode, ignoreRecord)(self);
                },
                _ => {
                    std.log.warn("unknown block ID {}", .{block_id});
                    try blockLoop(DummyCode, ignoreRecord)(self);
                },
            }
        }

        const DummyCode = enum(u32) { _ };

        fn ignoreRecord(self: *Self, rc: DummyCode) anyerror!void {
            _ = self;
            _ = rc;
        }

        fn parseModuleRecord(self: *Self, rc: Bitcode.Module.Code) !void {
            switch (rc) {
                .MODULE_CODE_ALIAS => {
                    std.log.warn("TODO: module record code {s}", .{@tagName(rc)});
                },
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
                    self.bc.module.version = try self.parseOp(u2);
                },
                .MODULE_CODE_TRIPLE => {
                    if (self.bc.module.triple.len != 0) {
                        return error.InvalidBitcode;
                    }
                    self.bc.module.triple = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                },
                .MODULE_CODE_DATALAYOUT => {
                    if (self.bc.module.data_layout.len != 0) {
                        return error.InvalidBitcode;
                    }
                    self.bc.module.data_layout = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                },
                .MODULE_CODE_SOURCE_FILENAME => {
                    if (self.bc.module.source_filename.len != 0) {
                        return error.InvalidBitcode;
                    }
                    self.bc.module.source_filename = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                },
                .MODULE_CODE_ASM => {
                    if (self.bc.module.@"asm".len != 0) {
                        return error.InvalidBitcode;
                    }
                    self.bc.module.@"asm" = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                },
                .MODULE_CODE_SECTIONNAME => {
                    const name = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                    try self.appendOne([]const u8, &self.bc.module.section_name, name);
                },
                .MODULE_CODE_DEPLIB => {
                    const name = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                    try self.appendOne([]const u8, &self.bc.module.deplib, name);
                },
                .MODULE_CODE_GCNAME => {
                    const name = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                    try self.appendOne([]const u8, &self.bc.module.gc_name, name);
                },
                _ => {
                    std.log.warn("unknown module record code {}", .{rc});
                },
            }
        }

        fn parseIdentificationRecord(self: *Self, rc: Bitcode.Idendification.Code) !void {
            switch (rc) {
                .IDENTIFICATION_CODE_STRING => {
                    self.bc.identification.string = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                },
                .IDENTIFICATION_CODE_EPOCH => {
                    self.bc.identification.epoch = try self.parseOp(u0);
                },
                _ => {
                    std.log.warn("unknown identification code {}", .{rc});
                },
            }
        }

        fn parseStrtabRecord(self: *Self, rc: Bitcode.Strtab.Code) !void {
            switch (rc) {
                .STRTAB_BLOB => {
                    self.bc.strtab.contents = (try self.walker.recordBlob()).?;
                },
                _ => {
                    std.log.warn("unknown strtab code {}", .{rc});
                },
            }
        }

        fn parseTypeRecord(self: *Self, rc: Bitcode.Module.Type.Code) !void {
            switch (rc) {
                .TYPE_CODE_NUMENTRY => {
                    if (self.bc.module.type.entries.len != 0) {
                        std.log.err("duplicate NUMENTRY for type table", .{});
                        return error.InvalidBitcode;
                    }
                    const count = try self.parseOp(usize);
                    self.bc.module.type.entries = try self.gpa.alloc(Bitcode.Module.Type.Entry, count);
                },
                .TYPE_CODE_VOID => return try self.appendTypeDefinition(.void),
                .TYPE_CODE_FLOAT => return try self.appendTypeDefinition(.float),
                .TYPE_CODE_DOUBLE => return try self.appendTypeDefinition(.double),
                .TYPE_CODE_LABEL => return try self.appendTypeDefinition(.label),
                .TYPE_CODE_OPAQUE => return try self.appendTypeDefinitionNamed(.{ .@"opaque" = undefined }),
                .TYPE_CODE_INTEGER => {
                    const width = try self.parseOp(u16);
                    return try self.appendTypeDefinition(.{ .integer = width });
                },
                .TYPE_CODE_POINTER => {
                    const t = Bitcode.Module.Type.Entry{ .pointer = .{
                        .pointee_type_index = try self.parseOp(u32),
                        .address_space = try self.parseOp(u16),
                    } };
                    return try self.appendTypeDefinition(t);
                },
                .TYPE_CODE_HALF => return try self.appendTypeDefinition(.float),
                .TYPE_CODE_ARRAY => {
                    return try self.appendTypeDefinition(.{ .array = .{
                        .element_count = try self.parseOp(u64),
                        .element_type_index = try self.parseOp(u32),
                    } });
                },
                .TYPE_CODE_VECTOR => {
                    return try self.appendTypeDefinition(.{ .vector = .{
                        .element_count = try self.parseOp(u64),
                        .element_type_index = try self.parseOp(u32),
                    } });
                },
                .TYPE_CODE_X86_FP80 => return try self.appendTypeDefinition(.x86_fp80),
                .TYPE_CODE_FP128 => return try self.appendTypeDefinition(.fp128),
                .TYPE_CODE_PPC_FP128 => return try self.appendTypeDefinition(.ppc_fp128),
                .TYPE_CODE_METADATA => return try self.appendTypeDefinition(.metadata),
                .TYPE_CODE_X86_MMX => return try self.appendTypeDefinition(.x86_mmx),
                .TYPE_CODE_STRUCT_ANON => {
                    return try self.appendTypeDefinition(.{ .@"struct" = .{
                        .name = null,
                        .is_packed = try self.parseOp(bool),
                        .element_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.gpa)).?,
                    } });
                },
                .TYPE_CODE_STRUCT_NAME => {
                    self.found.pending_type_name = (try self.walker.remainingRecordValuesAlloc(u8, self.gpa)).?;
                },
                .TYPE_CODE_STRUCT_NAMED => {
                    return try self.appendTypeDefinitionNamed(.{ .@"struct" = .{
                        .name = undefined,
                        .is_packed = try self.parseOp(bool),
                        .element_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.gpa)).?,
                    } });
                },
                .TYPE_CODE_FUNCTION => {
                    return try self.appendTypeDefinition(.{ .function = .{
                        .is_vararg = try self.parseOp(bool),
                        .return_type_index = try self.parseOp(u32),
                        .param_type_indexes = (try self.walker.remainingRecordValuesAlloc(u32, self.gpa)).?,
                    } });
                },
                .TYPE_CODE_BFLOAT => return try self.appendTypeDefinition(.bfloat),
                .TYPE_CODE_X86_AMX => return try self.appendTypeDefinition(.x86_amx),
                _ => std.log.warn("unknown type code {}", .{rc}),
            }
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
            slice.* = try self.gpa.realloc(slice.*, len + 1);
            slice.*[len] = val;
        }
    };
}
