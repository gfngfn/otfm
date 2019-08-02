(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** OpenType font decoder.

    {b WARNING.} This interface is subject to change in the future.

    [Otfm] is an in-memory decoder for the OpenType font data format.
    It provides low-level access to OpenType fonts tables and functions
    to decode some of them.

    Consult the {{!limitations}limitations} and {{!examples}example} of
    use.

    {b Note.} Unless otherwise specified the strings returned are
    UTF-8 encoded.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}

    {3 References}
    {ul
    {- Microsoft.
    {e {{:https://www.microsoft.com/typography/otspec/default.htm}
        The OpenType Specification}}, 2009.}} *)

open Result

open OtfTypes
open OtfUtils
open OtfDecBasic


(** {1 Tags} *)

type tag = OtfTag.t
(** The type for OpenType tags. *)

(** {1 Table decoding}

    These functions can be used in any order and are robust: when they
    return an error the decoder is back to a consistant state and can
    be used further. However if {!flavour} or {!table_list} returns an
    error you can safely assume that all other functions will. The
    fields are in general not documented please refer to the OpenType
    {{:https://www.microsoft.com/typography/otspec/default.htm}
    specification} for details. *)

(** {2:cmap cmap table} *)

type glyph_id = int
(** The type for glyph ids, from [0] to [65534]. *)

type map_kind = [ `Glyph | `Glyph_range ]
(** The type for map kinds.

    Determines how an unicode range [(u0, u1)] and a glyph id [gid]
    must be interpreted in the folding function of {!cmap}.
    {ul
    {- [`Glyph] all characters in the range map to to [gid].}
    {- [`Glyph_range], [u0] maps to [gid], [u0 + 1] to [gid + 1], ...
       and [u1] to [gid + (u1 - u0)]}} *)

type cmap_subtable_ids = {
  platform_id : int;
  encoding_id : int;
  format      : int;
}

type cmap_subtable
(** The type for cmap subtables. *)

val cmap : decoder -> (cmap_subtable list, error) result
(** [cmap d] returns the list of all
    {{:https://www.microsoft.com/typography/otspec/cmap.htm}cmap} subtables in the font. *)

val cmap_subtable_ids : cmap_subtable -> cmap_subtable_ids

val cmap_subtable : cmap_subtable -> ('a -> map_kind -> cp_range -> glyph_id -> 'a) -> 'a -> ('a, error) result
(** [cmap_subtable st f acc] folds over a mapping from unicode
    scalar values to glyph ids by reading the cmap subtable [st].

    {b Limitations.} Only the format 13 (last resort font), format 12
    (UCS-4) and format 4 (UCS-2) cmap table formats are supported.
*)

(** {2:glyf glyf table} *)

type glyf_loc
(** The type for glyph locations. See {!loca} table. *)

type glyph_simple_descr = (bool * int * int) list list
(** The type for simple glyph descriptions. Lists of contours, contours
    are list of points with a boolean indicating whether the point is
    on or off curve. *)

type glyph_composite_descr =
  (glyph_id * (int * int) * (float * float * float * float) option) list
(** The type for glyph composites. A list of components made of
    a glyph id, a translation and an optional linear transform [a b c d]
    (column major). *)

type glyph_descr =
  [ `Simple of glyph_simple_descr
  | `Composite of glyph_composite_descr ] * (int * int * int * int)
(** The type for glyph descriptions. A simple or composite descriptions
    with the glyph's [(minx, miny, maxx, maxy)]'s bounding box. *)

val glyf : decoder -> glyf_loc -> (glyph_descr option, error) result
(** [glyf d loc] is the glyph descroption located at [loc] by reading
    the {{:https://www.microsoft.com/typography/otspec/glyf.htm}glyf}
    table. Glyph locations are obtainted via {!loca}. *)

(** {2:head head table} *)

type head = {
  head_font_revision       : WideInt.t;
  head_flags               : int;
  head_units_per_em        : int;
  head_created             : WideInt.t;  (** Unix timestamp. *)
  head_modified            : WideInt.t;  (** Unix timestamp. *)
  head_xmin                : int;
  head_ymin                : int;
  head_xmax                : int;
  head_ymax                : int;
  head_mac_style           : int;
  head_lowest_rec_ppem     : int;
  head_index_to_loc_format : loc_format;
}
(** The type for representing
    {{:https://www.microsoft.com/typography/otspec/head.htm}head} tables. *)

val head : decoder -> (head, error) result
(** [head d] is the head table. *)

(** {2:hhea hhea table} *)

type hhea = {
  hhea_ascender               : int;
  hhea_descender              : int;
  hhea_line_gap               : int;
  hhea_advance_width_max      : int;
  hhea_min_left_side_bearing  : int;
  hhea_min_right_side_bearing : int;
  hhea_xmax_extent            : int;
  hhea_caret_slope_rise       : int;
  hhea_caret_slope_run        : int;
  hhea_caret_offset           : int;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/hhea.htm}hhea} tables. *)

val hhea : decoder -> (hhea, error) result
(** [hhea d] is the hhea table. *)

(** {2:hmtx hmtx table} *)

val hmtx : decoder -> ('a -> glyph_id -> int -> int -> 'a) -> 'a -> ('a, error) result
(** [hmtx d f acc] folds over the horizontal metrics of the font by
    reading the
    {{:https://www.microsoft.com/typography/otspec/hmtx.htm}hmtx}
    table.  [f] is applied on each entry with [f acc' gid adv lsb]
    with [gid] the glyph id (guaranteed to range, in order, from
    [0] to glyph count minus one), [adv] the (unsigned) advance width,
    and [lsb] the (signed) left side bearing. *)

(** {2:maxp max table} *)

type maxp = {
  maxp_num_glyphs               : int;
  maxp_max_points               : int;
  maxp_max_contours             : int;
  maxp_max_composite_points     : int;
  maxp_max_composite_contours   : int;
  maxp_max_zones                : int;
  maxp_max_twilight_points      : int;
  maxp_max_storage              : int;
  maxp_max_function_defs        : int;
  maxp_max_instruction_defs     : int;
  maxp_max_stack_elements       : int;
  maxp_max_size_of_instructions : int;
  maxp_max_component_elements   : int;
  maxp_max_component_depth      : int;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/maxp.htm}maxp} tables. *)

val maxp : decoder -> (maxp, error) result
(** [maxp d] is the maxp table. *)

(** {2:name name table} *)

type lang = string
(** The type for {{:http://tools.ietf.org/html/bcp47}BCP 47} language tags. *)

val name : decoder -> ('a -> int -> lang -> string -> 'a) -> 'a -> ('a, error) result
(** [name d f acc] folds over the name records of the font by
    reading the {{:https://www.microsoft.com/typography/otspec/name.htm}name}
    table. [f] is applied on each name id entry with [f acc' nid lang name]
    with [nid] the name id, lang the language tag, and [name] the UTF-8
    encoded name value.

    {b Note.} The module normalizes Windows language ids to lowercased
    BCP 47 ids. Language tags found in language tag records should be
    BCP 47 language tags but are not checked for conformance.

    {b Tip.} If you are looking for the postcript name use
    {!postscript_name}.

    {b Limitations.} Lookups data only in platform ids 0, 2 and 3 (Unicode,
    ISO and Windows) with UTF-16BE encoding and reports only the data of
    the first one it finds for a given name id. *)

(** {2:os2 OS/2 table} *)

type os2 = {
  os2_x_avg_char_width       : int;
  os2_us_weight_class        : int;
  os2_us_width_class         : int;
  os2_fs_type                : int;
  os2_y_subscript_x_size     : int;
  os2_y_subscript_y_size     : int;
  os2_y_subscript_x_offset   : int;
  os2_y_subscript_y_offset   : int;
  os2_y_superscript_x_size   : int;
  os2_y_superscript_y_size   : int;
  os2_y_superscript_x_offset : int;
  os2_y_superscript_y_offset : int;
  os2_y_strikeout_size       : int;
  os2_y_strikeout_position   : int;
  os2_family_class           : int;
  os2_panose                 : string;  (** 10 bytes *)
  os2_ul_unicode_range1      : WideInt.t;
  os2_ul_unicode_range2      : WideInt.t;
  os2_ul_unicode_range3      : WideInt.t;
  os2_ul_unicode_range4      : WideInt.t;
  os2_ach_vend_id            : WideInt.t;
  os2_fs_selection           : int;
  os2_us_first_char_index    : int;
  os2_us_last_char_index     : int;
  os2_s_typo_ascender        : int;
  os2_s_type_descender       : int;
  os2_s_typo_linegap         : int;
  os2_us_win_ascent          : int;
  os2_us_win_descent         : int;
  os2_ul_code_page_range_1   : WideInt.t option;
  os2_ul_code_page_range_2   : WideInt.t option;
  os2_s_x_height             : int option;
  os2_s_cap_height           : int option;
  os2_us_default_char        : int option;
  os2_us_break_char          : int option;
  os2_us_max_context         : int option;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/os2.htm}OS/2} tables. *)

val os2 : decoder -> (os2, error) result
(** [os2 d] is the OS/2 table. *)

(** {2:kern kern table} *)

type kern_info = {
  kern_dir          : [ `H | `V ];
  kern_kind         : [ `Min | `Kern ];
  kern_cross_stream : bool;
}
(** The type for kerning (sub)table information. *)

val kern : decoder ->
  ('a -> kern_info -> [`Skip | `Fold ] * 'a) ->
  ('a -> glyph_id -> glyph_id -> int -> 'a) -> 'a ->
  ('a, error) result
(** [kern d t p acc] folds over the kerning tables of [d] by
    reading the {{:https://www.microsoft.com/typography/otspec/kern.htm}kern}
    table. [t] is called on each new (sub)table, the table pairs are skipped if
    it returns [`Skip] otherwise [p acc' left right value] is called on
    each kerning pair of the table. The function returns [acc] if there
    is no kern table.

    {b Limitations.} Only format 0 kerning tables are supported. *)

(** {2:loca loca table} *)

val loca : decoder -> glyph_id -> (glyf_loc option, error) result
(** [loca d gid] looks up the location of the glyph with id [gid] by
    reading the {{:https://www.microsoft.com/typography/otspec/loca.htm}loca}
    table. The result can be used with {!val:glyf} to lookup the glyph. *)

(** {1:limitations Limitations}

    As it stands [Otfm] has the following limitations.  Some of these
    may be lifted in the future and a few of these can be overcome
    by pre-processing your font (e.g. extract [.ttc] files to [.ttf], or
    removing hinting information to reduce the font size). See also
    the individual table decoding functions for other limitations.

    {ul
    {- True Type collections ([.ttc] files) are not supported}
    {- The whole font needs to be loaded in memory as a string. This may
       be a limiting factor on 32 bits platforms (but non [.ttc] font
       files tend to be smaller than 16 Mo).}
    {- Table checksums are not verified.}}
*)

(** {1:examples Examples}

    The following code prints the postscript name of the font
    on stdout.
{[
  let otf_postscript_name bytes =
    let d = Otfm.decoder (`String bytes) in
    match Otfm.postscript_name d with
    | Error e -> Format.eprintf "@[%a@]@." Otfm.pp_error e
    | Ok (Some n) -> Format.printf "%s@." n;
    | Ok None -> ()
]}
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

type gsub_script
(** The type for GSUB Script. *)

type gsub_langsys
(** The type for GSUB LangSys. *)

type gsub_feature
(** The type for GSUB Feature. *)

val gsub_script : decoder -> ((gsub_script list) option, error) result
(** [gsub_script d] returns all of the Script tags the font contains. *)

val gsub_script_tag : gsub_script -> string
(** Returns the Script tag (e.g. ["arab"], ["cyrl"], ["hani"], ["latn"], etc.). *)

val gsub_langsys : gsub_script -> (gsub_langsys option * gsub_langsys list, error) result
(** [gsub_langsys gs] returns
    the pair of DefaultLangSys and all of the LangSys tags for the given Script [gs]. *)

val gsub_langsys_tag : gsub_langsys -> string
(** Returns the LangSys tag (e.g. [Some "DEU"], [Some "JAN"], [Some "TRK"], etc.). *)

val gsub_feature : gsub_langsys -> (gsub_feature option * gsub_feature list, error) result
(** [gsub_feature gl] returns
    the pair of RequiredFeatureTag and all of the Feature tags for the given LangSys [gl]. *)

val gsub_feature_tag : gsub_feature -> string
(** Returns the Feature tag (e.g. ["aalt"], ["liga"], etc.). *)

type 'a folding_gsub_single = 'a -> glyph_id * glyph_id -> 'a

type 'a folding_gsub_alt = 'a -> glyph_id * glyph_id list -> 'a

type 'a folding_gsub_lig = 'a -> glyph_id * (glyph_id list * glyph_id) list -> 'a

val gsub : gsub_feature ->
  ?single:'a folding_gsub_single ->
  ?alt:'a folding_gsub_alt ->
  ?lig:'a folding_gsub_lig ->
  'a -> ('a, error) result
(** {b WARNING: subject to change in the future.}
    Supports only
    {{:https://www.microsoft.com/typography/otspec/gsub.htm#LS}LookupType 4: ligature substitution subtable}.
    [gsub feature f init] folds the substitution subtables corresponding to [feature]. *)

type gpos_script
(** The type for GPOS Script. *)

type gpos_langsys
(** The type for GPOS LangSys. *)

type gpos_feature
(** The type for GPOS Feature. *)

val gpos_script : decoder -> ((gpos_script list) option, error) result
(** [gpos_script d] returns all of the Script tags the font contains. *)

val gpos_script_tag : gpos_script -> string
(** Returns the Script tag (e.g. ["arab"], ["cyrl"], ["hani"], ["latn"], etc.). *)

val gpos_langsys : gpos_script -> (gpos_langsys option * gpos_langsys list, error) result
(** [gpos_langsys gs] returns
    the pair of DefaultLangSys and all of the LangSys tags for the given Script [gs]. *)

val gpos_langsys_tag : gpos_langsys -> string
(** Returns the LangSys tag (e.g. [Some "DEU"], [Some "JAN"], [Some "TRK"], etc.). *)

val gpos_feature : gpos_langsys -> (gpos_feature option * gpos_feature list, error) result
(** [gpos_feature gl] returns
    the pair of RequiredFeatureTag and all of the Feature tags for the given LangSys [gl]. *)

val gpos_feature_tag : gpos_feature -> string
(** Returns the Feature tag (e.g. ["aalt"], ["liga"], etc.). *)

type value_record = {
  x_placement  : int option;
  y_placement  : int option;
  x_advance    : int option;
  y_advance    : int option;
  x_pla_device : int option;
  y_pla_device : int option;
  x_adv_device : int option;
  y_adv_device : int option;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/gpos.htm#valueRecord}ValueRecord} tables. *)

type class_value = int

type class_definition =
  | GlyphToClass      of glyph_id * class_value
  | GlyphRangeToClass of glyph_id * glyph_id * class_value

type anchor_adjustment =
  | NoAnchorAdjustment
  | AnchorPointAdjustment  of int
  | DeviceAnchorAdjustment of device_table * device_table

type design_units = int

type anchor = design_units * design_units * anchor_adjustment

type mark_class = int

type mark_record = mark_class * anchor

type base_record = (anchor option) array
  (* -- indexed by mark_class -- *)
  (* -- UNDOCUMENTED (in OpenType 1.8.3) -- *)
  (* -- BaseRecord tables can contain NULL pointers. -- *)

type component_record = (anchor option) array
  (* -- indexed by mark_class -- *)

type ligature_attach = component_record list

type mark2_record = anchor array
  (* -- indexed by mark_class -- *)

type 'a folding_gpos_single1 = 'a -> glyph_id list -> value_record -> 'a

type 'a folding_gpos_single2 = 'a -> glyph_id * value_record -> 'a

type 'a folding_gpos_pair1 = 'a -> glyph_id * (glyph_id * value_record * value_record) list -> 'a

type 'a folding_gpos_pair2 = class_definition list -> class_definition list -> 'a -> (class_value * (class_value * value_record * value_record) list) list -> 'a

type 'a folding_gpos_markbase1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * base_record) list -> 'a

type 'a folding_gpos_marklig1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * ligature_attach) list -> 'a

type 'a folding_gpos_markmark1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * mark2_record) list -> 'a

val gpos : gpos_feature ->
  ?single1:'a folding_gpos_single1 ->
  ?single2:'a folding_gpos_single2 ->
  ?pair1:'a folding_gpos_pair1 ->
  ?pair2:'a folding_gpos_pair2 ->
  ?markbase1: 'a folding_gpos_markbase1 ->
  ?marklig1: 'a folding_gpos_marklig1 ->
  ?markmark1: 'a folding_gpos_markmark1 ->
  'a -> ('a, error) result

type math_value_record = int * device_table option
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathValueRecord} tables. *)

type math_constants =
  {
    script_percent_scale_down                     : int;
    script_script_percent_scale_down              : int;
    delimited_sub_formula_min_height              : int;
    display_operator_min_height                   : int;
    math_leading                                  : math_value_record;
    axis_height                                   : math_value_record;
    accent_base_height                            : math_value_record;
    flattened_accent_base_height                  : math_value_record;
    subscript_shift_down                          : math_value_record;
    subscript_top_max                             : math_value_record;
    subscript_baseline_drop_min                   : math_value_record;
    superscript_shift_up                          : math_value_record;
    superscript_shift_up_cramped                  : math_value_record;
    superscript_bottom_min                        : math_value_record;
    superscript_baseline_drop_max                 : math_value_record;
    sub_superscript_gap_min                       : math_value_record;
    superscript_bottom_max_with_subscript         : math_value_record;
    space_after_script                            : math_value_record;
    upper_limit_gap_min                           : math_value_record;
    upper_limit_baseline_rise_min                 : math_value_record;
    lower_limit_gap_min                           : math_value_record;
    lower_limit_baseline_drop_min                 : math_value_record;
    stack_top_shift_up                            : math_value_record;
    stack_top_display_style_shift_up              : math_value_record;
    stack_bottom_shift_down                       : math_value_record;
    stack_bottom_display_style_shift_down         : math_value_record;
    stack_gap_min                                 : math_value_record;
    stack_display_style_gap_min                   : math_value_record;
    stretch_stack_top_shift_up                    : math_value_record;
    stretch_stack_bottom_shift_down               : math_value_record;
    stretch_stack_gap_above_min                   : math_value_record;
    stretch_stack_gap_below_min                   : math_value_record;
    fraction_numerator_shift_up                   : math_value_record;
    fraction_numerator_display_style_shift_up     : math_value_record;
    fraction_denominator_shift_down               : math_value_record;
    fraction_denominator_display_style_shift_down : math_value_record;
    fraction_numerator_gap_min                    : math_value_record;
    fraction_num_display_style_gap_min            : math_value_record;
    fraction_rule_thickness                       : math_value_record;
    fraction_denominator_gap_min                  : math_value_record;
    fraction_denom_display_style_gap_min          : math_value_record;
    skewed_fraction_horizontal_gap                : math_value_record;
    skewed_fraction_vertical_gap                  : math_value_record;
    overbar_vertical_gap                          : math_value_record;
    overbar_rule_thickness                        : math_value_record;
    overbar_extra_ascender                        : math_value_record;
    underbar_vertical_gap                         : math_value_record;
    underbar_rule_thickness                       : math_value_record;
    underbar_extra_descender                      : math_value_record;
    radical_vertical_gap                          : math_value_record;
    radical_display_style_vertical_gap            : math_value_record;
    radical_rule_thickness                        : math_value_record;
    radical_extra_ascender                        : math_value_record;
    radical_kern_before_degree                    : math_value_record;
    radical_kern_after_degree                     : math_value_record;
    radical_degree_bottom_raise_percent           : int;
  }
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathConstants} tables. *)

type math_kern = math_value_record list * math_value_record list
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathKern} tables. *)

type math_kern_info_record = {
  top_right_math_kern    : math_kern option;
  top_left_math_kern     : math_kern option;
  bottom_right_math_kern : math_kern option;
  bottom_left_math_kern  : math_kern option;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathKernInfoRecord} tables. *)

type math_glyph_info = {
  math_italics_correction    : (glyph_id * math_value_record) list;
  math_top_accent_attachment : (glyph_id * math_value_record) list;
  math_kern_info             : (glyph_id * math_kern_info_record) list;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathGlyphInfo} tables. *)

type glyph_part_record = {
  glyph_id_for_part      : glyph_id;
  start_connector_length : int;
  end_connector_length   : int;
  full_advance           : int;
  part_flags             : int;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}GlyphPartRecord} tables. *)

type math_glyph_construction = {
  glyph_assembly                 : (math_value_record * glyph_part_record list) option;
  math_glyph_variant_record_list : (glyph_id * int) list;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathGlyphConstruction} tables. *)

type math_variants = {
  min_connector_overlap : int;
  vert_glyph_assoc      : (glyph_id * math_glyph_construction) list;
  horiz_glyph_assoc     : (glyph_id * math_glyph_construction) list;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MathVariants} tables. *)

type math = {
  math_constants  : math_constants;
  math_glyph_info : math_glyph_info;
  math_variants   : math_variants;
}
(** The type for
    {{:https://www.microsoft.com/typography/otspec/math.htm}MATH} tables. *)

val math : decoder -> (math option, error) result
(** [math d] returns the whole information in the
    {{:https://www.microsoft.com/typography/otspec/math.htm}MATH} table of [d]. *)

type cff_cid_info = {
  registry          : string;
  ordering          : string;
  supplement        : int;
  cid_font_version  : float;
  cid_font_revision : int;
  cid_font_type     : int;
  cid_count         : int;
}

type charstring_info

type cff_first

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

val cff : decoder -> (cff_info option, error) result

type charstring_element

type csx = int

type csy = int

type cspoint = csx * csy

type stem_argument = string  (* temporary *)

type parsed_charstring =
  | HStem     of int * int * cspoint list                                         (** hstem (1) *)
  | VStem     of int * int * cspoint list                                         (** vstem (3) *)
  | VMoveTo   of int                                                              (** vmoveto (4) *)
  | RLineTo   of cspoint list                                                     (** rlineto (5) *)
  | HLineTo   of int list                                                         (** hlineto (6) *)
  | VLineTo   of int list                                                         (** vlineto (7) *)
  | RRCurveTo of (cspoint * cspoint * cspoint) list                               (** rrcurveto (8) *)
  | HStemHM   of int * int * cspoint list                                         (** hstemhm (18) *)
  | HintMask  of stem_argument                                                    (** hintmask (19) *)
  | CntrMask  of stem_argument                                                    (** cntrmask (20) *)
  | RMoveTo   of cspoint                                                          (** rmoveto (21) *)
  | HMoveTo   of int                                                              (** hmoveto (22) *)
  | VStemHM   of int * int * cspoint list                                         (** vstemhm (23) *)
  | VVCurveTo of csx option * (csy * cspoint * csy) list                          (** vvcurveto (26) *)
  | HHCurveTo of csy option * (csx * cspoint * csx) list                          (** hhcurveto (27) *)
  | VHCurveTo of (int * cspoint * int) list * int option                          (** vhcurveto (30) *)
  | HVCurveTo of (int * cspoint * int) list * int option                          (** hvcurveto (31) *)
  | Flex      of cspoint * cspoint * cspoint * cspoint * cspoint * cspoint * int  (** flex (12 35) *)
  | HFlex     of int * cspoint * int * int * int * int                            (** hflex (12 34) *)
  | HFlex1    of cspoint * cspoint * int * int * cspoint * int                    (** hflex1 (12 36) *)
  | Flex1     of cspoint * cspoint * cspoint * cspoint * cspoint * int            (** flex1 (12 37) *)

val pp_parsed_charstring : Format.formatter -> parsed_charstring -> unit

val charstring : charstring_info -> glyph_id -> (((int option * parsed_charstring list) option), error) result  (* temporary *)

type path_element =
  | LineTo         of cspoint
  | BezierTo       of cspoint * cspoint * cspoint

type path = cspoint * path_element list

val charstring_absolute : charstring_info -> glyph_id -> ((path list) option, error) result

val charstring_bbox : path list -> (csx * csx * csy * csy) option

type raw_glyph

val get_raw_glyph : decoder -> cff_info option -> glyph_id -> (raw_glyph option, error) result


module Encode : sig

  type raw_table

  type outline_type =
    | TrueTypeOutline
    | CFFData

  val make_font_file : outline_type -> raw_table list -> (string, error) result

  val empty_cmap : unit -> (raw_table, error) result

  val head : head -> (raw_table, error) result

  val hhea : int -> hhea -> (raw_table, error) result

  val maxp : outline_type -> maxp -> (raw_table, error) result

  type glyph_output_info = {

  (* -- main table data -- *)
    hmtx : raw_table;

  (* -- for 'maxp' table -- *)
    number_of_glyphs : int;

  (* -- for 'head' table -- *)
    xmin                : int;
    ymin                : int;
    xmax                : int;
    ymax                : int;
    index_to_loc_format : loc_format;

  (* -- for 'hhea' table -- *)
    advance_width_max      : int;
    min_left_side_bearing  : int;
    min_right_side_bearing : int;
    x_max_extent           : int;
    number_of_h_metrics    : int;
  }

  type glyph_data =
    | TrueTypeGlyph of raw_table * raw_table
        (* glyf, loca *)
    | CFFGlyph      of raw_table
        (* CFF *)

  val truetype_outline_tables : raw_glyph list -> ((glyph_output_info * glyph_data), error) result

  val cff_outline_tables : decoder -> cff_info -> raw_glyph list -> ((glyph_output_info * glyph_data), error) result

end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Takashi Suwa

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
