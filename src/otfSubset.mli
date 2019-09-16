open OtfTypes

val make_cff : OtfDecBasic.cff_decoder -> glyph_id list -> (string option, OtfError.t) result
