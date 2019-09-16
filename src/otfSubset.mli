open OtfTypes

val make_cff : OtfDecCFF.cff_decoder -> glyph_id list -> (string option, OtfError.t) result
