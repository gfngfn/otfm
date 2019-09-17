
open OtfTypes
open OtfCFFTypes
open OtfUtils
open OtfDecBasic

type cff_decoder

val cff_common : cff_decoder -> common_decoder

val make_initial_cff : common_decoder -> cff_decoder


type cff_raw_glyph

val cff_raw_glyph : cff_raw_glyph -> raw_glyph

val get_cff_raw_glyph : cff_decoder -> glyph_id -> (cff_raw_glyph option, error) result

type charstring_info = cff_decoder * subroutine_index * private_info * int

type cff_info = {
  cff_first           : cff_first;
  font_name           : string;
  is_fixed_pitch      : bool;
  italic_angle        : int;
  underline_position  : int;
  underline_thickness : int;
  paint_type          : int;
  (* font_matrix : float * float * float * float; *)
  font_bbox           : int * int * int * int;
  stroke_width        : int;
  cid_info            : cff_cid_info option;
  number_of_glyphs    : int;
  charstring_info     : charstring_info;
}

val cff : cff_decoder -> cff_info ok

val pp_parsed_charstring : Format.formatter -> parsed_charstring -> unit

val charstring : charstring_info -> glyph_id -> (((int option * parsed_charstring list) option), error) result  (* temporary *)

type path_element =
  | LineTo         of cspoint
  | BezierTo       of cspoint * cspoint * cspoint

type path = cspoint * path_element list

val charstring_absolute : charstring_info -> glyph_id -> ((path list) option, error) result

val charstring_bbox : path list -> (csx * csx * csy * csy) option

val select_fd_index : fdselect -> glyph_id -> fdindex ok

val select_local_subr_index : private_info -> glyph_id -> subroutine_index ok

type width_state =
  | WidthLookedFor
  | WidthFound of int option

type charstring_state = {
  numarg        : int;
  numstem       : int;
  used_gsubr_set : IntSet.t;
  used_lsubr_set : IntSet.t;
}

val d_private_lsubr_pair_opt : int -> dict -> common_decoder -> ((dict * subroutine_index option) option) ok
(* FIXME; should be hidden in the module *)

val d_fontdict_private_pair_array : int -> dict -> common_decoder -> ((dict * (dict * subroutine_index option) option) array) ok
(* FIXME; should be hidden in the module *)

val parse_charstring : int -> charstring_state -> common_decoder -> int Stack.t -> subroutine_index -> subroutine_index -> width_state -> (width_state * charstring_state * parsed_charstring Alist.t) ok
(* FIXME; should be hidden in the module *)
