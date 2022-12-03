//! Defines the contents of an LLVM bitcode file
//! and constants used in the bitstream encoding.

const std = @import("std");
const assert = std.debug.assert;

const bitstream = @import("bitstream.zig");

/// Magic header byte that always appears at the start of LLVM bitcode.
pub const magic = [4]u8{ 'B', 'C', 0xc0, 0xde };

pub const BlockId = enum(std.meta.Tag(bitstream.BlockId)) {
    MODULE_BLOCK_ID = bitstream.BlockId.first_application_block_id,
    PARAMATTR_BLOCK_ID,
    PARAMATTR_GROUP_BLOCK_ID,
    CONSTANTS_BLOCK_ID,
    FUNCTION_BLOCK_ID,
    IDENTIFICATION_BLOCK_ID,
    VALUE_SYMTAB_BLOCK_ID,
    METADATA_BLOCK_ID,
    METADATA_ATTACHMENT_ID,
    TYPE_BLOCK_ID_NEW,
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

    pub fn fromBitstreamBlockId(from: bitstream.BlockId) BlockId {
        assert(@enumToInt(from) >= bitstream.BlockId.first_application_block_id);
        return @intToEnum(BlockId, @enumToInt(from));
    }
};

identification: Idendification = .{},
module: Module = .{},
symtab: Symtab = .{},
strtab: Strtab = .{},

pub const Idendification = struct {
    identification: []const u8 = "",
    epoch: u0 = 0, // TODO
};

pub const Module = struct {
    version: u2 = 0,
    type: Type = .{},
    param_attr_group: ParamAttrGroup = .{},
    param_attr: ParamAttr = .{},
    triple: []const u8 = "",
    data_layout: []const u8 = "",
    source_filename: []const u8 = "",
    global_var: []GlobalVar = &.{},
    function: []Function = &.{},
    vst_offset: void = {},
    constants: Constants = .{},

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

        MODULE_CODE_SOURCE_FILENAME = 16,

        _,
    };

    pub const Type = struct {
        // TODO
    };

    pub const ParamAttrGroup = struct {
        // TODO
    };

    pub const ParamAttr = struct {
        // TODO
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
        comdat: void, // TODO
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
            deprecated13,
            deprecated14,
        };

        pub const Visibility = enum(u2) {
            default,
            hidden,
            protected,
        };

        pub const Threadlocal = enum(u3) {
            no,
            default_tls_mode,
            local_dynamic,
            initial_exec,
            local_exec,
        };

        pub const UnnamedAddr = enum(u2) {
            no,
            unnamed_addr,
            local_unnamed_addr,
        };

        pub const DllStorageClass = enum(u2) {
            default,
            import,
            @"export",
        };

        pub const PreemptionSpecifier = enum(u1) {
            dso_preemptable,
            dso_local,
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
        comdat: void, // TODO
        prefix_index: ?u32,
        personality_fn_index: ?u32,
        preemption_specifier: GlobalVar.PreemptionSpecifier,

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
        };
    };

    pub const Constants = struct {
        // TODO
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
    // TODO
};
