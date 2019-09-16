
open OtfTypes
open OtfUtils
open OtfDecBasic

type ttf_decoder

val ttf_common : ttf_decoder -> common_decoder

val make_initial_ttf : common_decoder -> ttf_decoder

val init_glyf : ttf_decoder -> int ok
(** returns the byte offset of [d]'s 'glyf' table if exists, or [None] otherwise. *)

val d_loca_format : common_decoder -> loc_format ok

val init_loca : ttf_decoder -> (int * loc_format) ok
