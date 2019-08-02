open OtfTypes

val make : OtfDecBasic.decoder -> Otfm.cff_info option -> glyph_id list -> (string option, OtfError.t) result
