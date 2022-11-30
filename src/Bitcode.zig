//! Defines the contents of an LLVM bitcode file
//! and constants used in the bitstream encoding.

const std = @import("std");

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
};

identification: Idendification,
module: Module,
symtab: Symtab,
strtab: Strtab,

pub const Idendification = struct {
    identification: []const u8,
    epoch: u0, // TODO
};

pub const Module = struct {
    version: u2,
    type: Type,
    param_attr_group: ParamAttrGroup,
    param_attr: ParamAttr,
    triple: []const u8,
    data_layout: []const u8,
    source_filename: []const u8,
    global_var: []const u8,
    function: []const u8,
    vst_offset: void,
    constants: Constants,

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