//! Defines the contents of an LLVM bitcode file
//! and constants used in the bitstream encoding.

const std = @import("std");
const assert = std.debug.assert;

const bitstream = @import("bitstream.zig");
const codes = bitstream.codes;

/// Magic header byte that always appears at the start of LLVM bitcode.
pub const magic = [4]u8{ 'B', 'C', 0xc0, 0xde };

pub const BlockId = enum(std.meta.Tag(codes.block.Id)) {
    MODULE_BLOCK_ID = codes.block.Id.first_application_id,
    PARAMATTR_BLOCK_ID,
    PARAMATTR_GROUP_BLOCK_ID,
    CONSTANTS_BLOCK_ID,
    FUNCTION_BLOCK_ID,
    IDENTIFICATION_BLOCK_ID,
    VALUE_SYMTAB_BLOCK_ID,
    METADATA_BLOCK_ID,
    METADATA_ATTACHMENT_ID,
    TYPE_BLOCK_ID,
    USELIST_BLOCK_ID,
    MODULE_STRTAB_BLOCK_ID,
    GLOBALVAL_SUMMARY_BLOCK_ID,
    OPERAND_BUNDLE_TAGS_BLOCK_ID,
    METADATA_KIND_BLOCK_ID,
    STRTAB_BLOCK_ID,
    FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID,
    SYMTAB_BLOCK_ID,
    SYNC_SCOPE_NAMES_BLOCK_ID,
    _,

    pub const last_known_block_id = BlockId.SYNC_SCOPE_NAMES_BLOCK_ID;

    pub fn fromBitstreamBlockId(from: codes.block.Id) BlockId {
        assert(@enumToInt(from) >= codes.block.Id.first_application_id);
        return @intToEnum(BlockId, @enumToInt(from));
    }
};

identification: Idendification = .{},
module: Module = .{},
symtab: Symtab = .{},
strtab: Strtab = .{},

pub const Idendification = struct {
    string: []const u8 = "",
    epoch: u0 = 0,

    pub const Code = enum(u2) {
        IDENTIFICATION_CODE_STRING = 1,
        IDENTIFICATION_CODE_EPOCH,
        _,
    };
};

pub const Module = struct {
    version: u2 = 0,
    type: Type = .{},
    param_attr_group: ParamAttrGroup = .{},
    param_attr: ParamAttr = .{},
    triple: []const u8 = "",
    data_layout: []const u8 = "",
    source_filename: []const u8 = "",
    // TODO: Encoding uses LF to separate asm blocks.
    // See how the consumer uses this and if it's useful to split them into separate slices.
    // Multiple `module asm` blocks are internally concatenated, so there should only be one in a .bc.
    // https://llvm.org/docs/LangRef.html#module-level-inline-assembly
    @"asm": []const u8 = "",
    section_name: [][]const u8 = &.{},
    deplib: [][]const u8 = &.{},
    gc_name: [][]const u8 = &.{},
    global_var: []GlobalVar = &.{},
    function: []Function = &.{},
    aliases: []Alias = &.{},
    vst_offset: struct {} = .{},
    constants: []Constant = &.{},

    pub const Code = enum(u32) {
        MODULE_CODE_VERSION = 1,
        MODULE_CODE_TRIPLE,
        MODULE_CODE_DATALAYOUT,
        MODULE_CODE_ASM,
        MODULE_CODE_SECTIONNAME,
        MODULE_CODE_DEPLIB,
        MODULE_CODE_GLOBALVAR,
        MODULE_CODE_FUNCTION,
        MODULE_CODE_ALIAS,

        MODULE_CODE_GCNAME = 11,
        // MODULE_CODE_COMDAT,
        // MODULE_CODE_VSTOFFSET,
        // MODULE_CODE_ALIAS,

        MODULE_CODE_SOURCE_FILENAME = 16,

        _,
    };

    pub const Type = struct {
        entries: []Entry = &.{},

        pub const Entry = union(enum) {
            void,
            float,
            double,
            label,
            @"opaque": Opaque,
            integer: u16, // width
            pointer: Pointer,
            half,
            array: Array,
            vector: Array,
            x86_fp80,
            fp128,
            ppc_fp128,
            metadata,
            x86_mmx,
            @"struct": Struct,
            function: Entry.Function,
            bfloat,
            x86_amx,

            pub const Opaque = struct {
                name: []const u8,
            };

            pub const Pointer = struct {
                pointee_type_index: u32,
                address_space: u16,
            };

            pub const Array = struct {
                element_count: u64,
                element_type_index: u32,
            };

            pub const Struct = struct {
                name: ?[]const u8, // Null for anonymous structs
                is_packed: bool,
                element_type_indexes: []u32,
            };

            pub const Function = struct {
                is_vararg: bool,
                return_type_index: u32,
                param_type_indexes: []u32,
            };

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };

        pub const Code = enum(u5) {
            TYPE_CODE_NUMENTRY = 1,
            TYPE_CODE_VOID,
            TYPE_CODE_FLOAT,
            TYPE_CODE_DOUBLE,
            TYPE_CODE_LABEL,
            TYPE_CODE_OPAQUE,
            TYPE_CODE_INTEGER,
            TYPE_CODE_POINTER,
            // TYPE_CODE_FUNCTION_OLD,
            TYPE_CODE_HALF = 10,
            TYPE_CODE_ARRAY,
            TYPE_CODE_VECTOR,
            TYPE_CODE_X86_FP80,
            TYPE_CODE_FP128,
            TYPE_CODE_PPC_FP128,
            TYPE_CODE_METADATA,
            TYPE_CODE_X86_MMX,
            TYPE_CODE_STRUCT_ANON,
            TYPE_CODE_STRUCT_NAME,
            TYPE_CODE_STRUCT_NAMED,
            TYPE_CODE_FUNCTION,

            TYPE_CODE_BFLOAT = 23,
            TYPE_CODE_X86_AMX,

            _,
        };
    };

    pub const ParamAttrGroup = struct {
        entries: []Entry = &.{},

        pub const Entry = struct {
            // TODO: ID seems to be consistently sequential.
            // Verify, and if so, validate ID during parse and remove this field.
            group_id: u32,
            param_idx: ParamIdx,
            attrs: []Attr,

            pub const ParamIdx = union(enum) {
                return_value,
                function,
                function_param: u32,

                pub const Code = enum(u32) {
                    return_value_attributes = 0,
                    function_attributes = 0xffffffff,
                    _,

                    pub fn toParamIdx(self: ParamIdx.Code) ParamIdx {
                        return switch (self) {
                            .return_value_attributes => .return_value,
                            .function_attributes => .function,
                            _ => |idx| .{ .function_param = @enumToInt(idx) - 1 },
                        };
                    }
                };

                pub const jsonStringify = defaultEnumJsonStringify(@This());
            };

            pub const Attr = union(enum) {
                well_known: union(Kind.WellKnown) {
                    @"align": u32,
                    alwaysinline,
                    byval,
                    inlinehint,
                    inreg,
                    minsize,
                    naked,
                    nest,
                    @"noalias",
                    nobuiltin,
                    nocapture,
                    nodeduplicate,
                    noimplicitfloat,
                    @"noinline",
                    nonlazybind,
                    noredzone,
                    noreturn,
                    nounwind,
                    optsize,
                    readnone,
                    readonly,
                    returned,
                    returns_twice,
                    signext,
                    alignstack: u32,
                    ssp,
                    sspreq,
                    sspstrong,
                    sret,
                    sanitize_address,
                    sanitize_thread,
                    sanitize_memory,
                    uwtable,
                    zeroext,
                    builtin,
                    cold,
                    optnone,
                    inalloca,
                    nonnull,
                    jumptable,
                    dereferenceable: u32,
                    dereferenceable_or_null: u32,
                    convergent,
                    safestack,
                    argmemonly,
                    swiftself,
                    swifterror,
                    norecurse,
                    inaccessiblememonly,
                    inaccessiblememonly_or_argmemonly,
                    allocsize: struct {
                        elem_size: u32,
                        num_elems: ?u32,
                    },
                    writeonly,
                    speculatable,
                    strictfp,
                    sanitize_hwaddress,
                    nocf_check,
                    optforfuzzing,
                    shadowcallstack,
                    speculative_load_hardening,
                    immarg,
                    willreturn,
                    nofree,
                    nosync,
                    sanitize_memtag,
                    preallocated,
                    no_merge,
                    null_pointer_is_valid,
                    noundef,
                    byref,
                    mustprogress,
                    vscale_range: struct {
                        min: u32,
                        max: ?u32,
                    },
                    swiftasync,
                    nosanitize_coverage,
                    elementtype,
                    disable_sanitizer_instrumentation,
                    nosanitize_bounds,
                },
                custom: struct {
                    attr: []const u8,
                    value: ?[]const u8,
                },

                pub const jsonStringify = todoJsonStringify(@This());

                pub const Kind = enum(u3) {
                    well_known = 0,
                    well_known_with_value = 1,
                    string = 3,
                    string_with_value = 4,
                    _,

                    pub const WellKnown = enum(u8) {
                        @"align" = 1,
                        alwaysinline = 2,
                        byval = 3,
                        inlinehint = 4,
                        inreg = 5,
                        minsize = 6,
                        naked = 7,
                        nest = 8,
                        @"noalias" = 9,
                        nobuiltin = 10,
                        nocapture = 11,
                        nodeduplicate = 12,
                        noimplicitfloat = 13,
                        @"noinline" = 14,
                        nonlazybind = 15,
                        noredzone = 16,
                        noreturn = 17,
                        nounwind = 18,
                        optsize = 19,
                        readnone = 20,
                        readonly = 21,
                        returned = 22,
                        returns_twice = 23,
                        signext = 24,
                        alignstack = 25,
                        ssp = 26,
                        sspreq = 27,
                        sspstrong = 28,
                        sret = 29,
                        sanitize_address = 30,
                        sanitize_thread = 31,
                        sanitize_memory = 32,
                        uwtable = 33,
                        zeroext = 34,
                        builtin = 35,
                        cold = 36,
                        optnone = 37,
                        inalloca = 38,
                        nonnull = 39,
                        jumptable = 40,
                        dereferenceable = 41,
                        dereferenceable_or_null = 42,
                        convergent = 43,
                        safestack = 44,
                        argmemonly = 45,
                        swiftself = 46,
                        swifterror = 47,
                        norecurse = 48,
                        inaccessiblememonly = 49,
                        inaccessiblememonly_or_argmemonly = 50,
                        allocsize = 51,
                        writeonly = 52,
                        speculatable = 53,
                        strictfp = 54,
                        sanitize_hwaddress = 55,
                        nocf_check = 56,
                        optforfuzzing = 57,
                        shadowcallstack = 58,
                        speculative_load_hardening = 59,
                        immarg = 60,
                        willreturn = 61,
                        nofree = 62,
                        nosync = 63,
                        sanitize_memtag = 64,
                        preallocated = 65,
                        no_merge = 66,
                        null_pointer_is_valid = 67,
                        noundef = 68,
                        byref = 69,
                        mustprogress = 70,

                        vscale_range = 74,
                        swiftasync = 75,
                        nosanitize_coverage = 76,
                        elementtype = 77,
                        disable_sanitizer_instrumentation = 78,
                        nosanitize_bounds = 79,
                    };
                };
            };
        };

        pub const Code = enum(u32) {
            PARAMATTR_GRP_CODE_ENTRY = 3,
            _,
        };
    };

    pub const ParamAttr = struct {
        attr_group_indexes: []u32 = &.{},

        pub const Code = enum(u32) {
            PARAMATTR_CODE_ENTRY = 2,
            _,
        };
    };

    pub const GlobalVar = struct {
        strtab_offset: u32,
        strtab_size: u32,
        pointer_type_index: u32,
        is_const: bool,
        init_id: ?u32,
        linkage: Linkage,
        alignment_log2: u16,
        section_index: ?u32,
        visibility: Visibility,
        @"threadlocal": Threadlocal,
        unnamed_addr: UnnamedAddr,
        externally_initialized: bool,
        dll_storage_class: DllStorageClass,
        comdat: u64, // TODO
        attributes_index: ?u32,
        preemption_specifier: PreemptionSpecifier,

        pub const Linkage = enum(u4) {
            external,
            weak,
            appending,
            internal,
            link_once,
            dll_import,
            dll_export,
            extern_weak,
            common,
            private,
            weak_odr,
            link_once_odr,
            available_externally,
            _,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };

        pub const Visibility = enum(u2) {
            default,
            hidden,
            protected,
            _,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };

        pub const Threadlocal = enum(u3) {
            no,
            default_tls_mode,
            local_dynamic,
            initial_exec,
            local_exec,
            _,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };

        pub const UnnamedAddr = enum(u2) {
            no,
            unnamed_addr,
            local_unnamed_addr,
            _,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };

        pub const DllStorageClass = enum(u2) {
            default,
            import,
            @"export",
            _,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };

        pub const PreemptionSpecifier = enum(u1) {
            dso_preemptable,
            dso_local,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };
    };

    pub const Function = struct {
        strtab_offset: u32,
        strtab_size: u32,
        type_index: u32,
        calling_conv: CallingConv,
        is_proto: bool,
        linkage: GlobalVar.Linkage,
        param_attr_index: ?u32,
        alignment_log2: u16,
        section_index: ?u32,
        visibility: GlobalVar.Visibility,
        gc_index: ?u32,
        unnamed_addr: GlobalVar.UnnamedAddr,
        prologue_data_index: ?u32,
        dll_storage_class: GlobalVar.DllStorageClass,
        comdat: u64, // TODO
        prefix_index: ?u32,
        personality_fn_index: ?u32,
        preemption_specifier: GlobalVar.PreemptionSpecifier,
        constants: []Constant = &.{},

        pub const CallingConv = enum(u7) {
            ccc = 0,
            fastcc = 8,
            coldcc = 9,
            webkit_jscc = 12,
            anyregcc = 13,
            preserve_mostcc = 14,
            preserve_allcc = 15,
            swiftcc = 16,
            cxx_fast_tlscc = 17,
            tailcc = 18,
            cfguard_checkcc = 19,
            swifttailcc = 20,
            x86_stdcallcc = 64,
            x86_fastcallcc = 65,
            arm_apcscc = 66,
            arm_aapcscc = 67,
            arm_aapcs_vfpcc = 68,

            pub const jsonStringify = defaultEnumJsonStringify(@This());
        };
    };

    pub const Alias = struct {
        strtab_offset: u32,
        strtab_size: u32,
        type_index: u32,
        aliasee_val_index: u32,
        linkage: GlobalVar.Linkage,
        visibility: GlobalVar.Visibility,
        dll_storage_class: GlobalVar.DllStorageClass,
        @"threadlocal": GlobalVar.Threadlocal,
        unnamed_addr: GlobalVar.UnnamedAddr,
        preemption_specifier: GlobalVar.PreemptionSpecifier,
    };

    pub const MetadataKind = struct {
        // TODO
    };

    pub const Metadata = struct {
        // TODO
    };
};

pub const Symtab = struct {
    // TODO
};

pub const Strtab = struct {
    contents: []const u8 = "",

    pub const Code = enum(u1) {
        STRTAB_BLOB = 1,
        _,
    };
};

pub const Constant = struct {
    type_index: u32,
    value: Value,

    /// TODO: For now, these just store raw bits
    pub const Value = union(enum) {
        undef,
        null,
        int: u64,
        wide_int: []u64,
        float: u64,
        aggregate: []u64, // TODO are these indexes or values?
        string: []u8,
        cstring: []u8,

        pub const jsonStringify = todoJsonStringify(@This());
    };

    // TODO: For now, just store the associated type index.
    // Later, consider resolving and storing the type from the index.
    pub const Type = u32;

    pub const Code = enum(u5) {
        CST_CODE_SETTYPE = 1, // sets type of constant (type_list[idx]), needs to be followed by another rec val
        CST_CODE_NULL,
        CST_CODE_UNDEF,
        CST_CODE_INTEGER,
        CST_CODE_WIDE_INTEGER,
        CST_CODE_FLOAT,
        CST_CODE_AGGREGATE,
        CST_CODE_STRING,
        CST_CODE_CSTRING,
        CST_CODE_CE_BINOP,
        CST_CODE_CE_CAST,
        CST_CODE_CE_GEP,
        CST_CODE_CE_SELECT,
        CST_CODE_CE_EXTRACTELT,
        CST_CODE_CE_INSERTELT,
        CST_CODE_CE_SHUFFLEVEC,
        CST_CODE_CE_CMP,
        CST_CODE_INLINEASM_OLD,
        CST_CODE_CE_SHUFVEC_EX,
        CST_CODE_CE_INBOUNDS_GEP,
        CST_CODE_BLOCKADDRESS,
        CST_CODE_DATA,
        CST_CODE_INLINEASM_OLD2,
        CST_CODE_CE_GEP_WITH_INRANGE_INDEX,
        CST_CODE_CE_UNOP,
        CST_CODE_POISON,
        CST_CODE_DSO_LOCAL_EQUIVALENT,
        CST_CODE_INLINEASM_OLD3,
        CST_CODE_NO_CFI_VALUE,
        CST_CODE_INLINEASM,
        _,
    };
};

fn defaultEnumJsonStringify(comptime T: type) fn (T, std.json.StringifyOptions, anytype) anyerror!void {
    return struct {
        pub fn stringify(self: T, opts: std.json.StringifyOptions, w: anytype) !void {
            try std.json.stringify(@tagName(self), opts, w);
        }
    }.stringify;
}

fn todoJsonStringify(comptime T: type) fn (T, std.json.StringifyOptions, anytype) anyerror!void {
    return struct {
        pub fn stringify(_: T, _: std.json.StringifyOptions, w: anytype) !void {
            _ = try w.write("\"TODO\"");
        }
    }.stringify;
}
