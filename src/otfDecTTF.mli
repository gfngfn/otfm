
open OtfTypes
open OtfUtils
open OtfDecBasic

type ttf_decoder

val ttf_common : ttf_decoder -> common_decoder
(** injection mapping. *)

val make_initial_ttf : common_decoder -> ttf_decoder
(** This is just for internal use but cannot be private. Do not use this. *)

val init_glyf : ttf_decoder -> int ok
(** returns the byte offset of [d]'s 'glyf' table if exists, or [None] otherwise. *)

val d_loca_format : common_decoder -> loc_format ok

val init_loca : ttf_decoder -> (int * loc_format) ok

(** {2:loca loca table} *)

val loca : ttf_decoder -> glyph_id -> (glyf_loc option, error) result
(** [loca d gid] looks up the location of the glyph with id [gid] by
    reading the {{:https://www.microsoft.com/typography/otspec/loca.htm}loca}
    table. The result can be used with {!val:glyf} to lookup the glyph. *)

(** {2:glyf glyf table} *)

val glyf : ttf_decoder -> glyf_loc -> (glyph_descr, error) result
(** [glyf d loc] is the glyph descroption located at [loc] by reading
    the {{:https://www.microsoft.com/typography/otspec/glyf.htm}glyf}
    table. Glyph locations are obtainted via {!loca}. *)

type ttf_raw_glyph

val ttf_raw_glyph : ttf_raw_glyph -> raw_glyph

val get_ttf_raw_glyph : ttf_decoder -> glyph_id -> (ttf_raw_glyph option, error) result
