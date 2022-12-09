//! Definitions of standard/builtin binary identifiers in bitstream files.
//! Enums are non-exhaustive because in most cases new codes are described
//! throughout a bitstream file.

pub const abbrev = struct {
    pub const Id = enum(u32) {
        END_BLOCK,
        ENTER_SUBBLOCK,
        DEFINE_ABBREV,
        UNABBREV_RECORD,
        _,

        /// All abbreviations defined with DEFINE_ABBREV must be this value or higher.
        pub const first_application_id = 4;
    };

    /// Binary codes for the definition of an encoded (non-literal) abbreviation operand.
    /// They only appear as a 3-bit int after the first bit that indicates the value is not a literal.
    pub const OpEncoding = enum(u3) {
        fixed = 1,
        vbr,
        array,
        char6,
        blob,
        /// Any other value is invalid.
        _,
    };
};

pub const block = struct {
    pub const Id = enum(u32) {
        BLOCKINFO,
        _,

        /// All application-specific headers must be this value or higher.
        pub const first_application_id = 8;
    };
};

pub const record = struct {
    pub const block_info = struct {
        pub const Id = enum(u32) {
            /// Sets which block ID following records apply to,
            /// instead of the current block (which is a BLOCKINFO).
            /// Must be the first record in a BLOCKINFO.
            BLOCKINFO_CODE_SETBID = 1,
            BLOCKINFO_CODE_BLOCKNAME,
            BLOCKINFO_CODE_SETRECORDNAME,
            /// BLOCKINFO does not allow for application-specific codes.
            _,
        };
    };
};
