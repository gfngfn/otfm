open OtfTypes

(* -- Unicode code points -- *)

let is_cp i = 0x0000 <= i && i <= 0x10FFFF
let pp_cp ppf cp = Format.fprintf ppf "U+%04X" cp

let pp = Format.fprintf

module Tag = OtfTag

type tag = Tag.t
type wint = WideInt.t

type context = [ `Table of tag | `Offset_table | `Table_directory ]

type t =
[
  | `Unknown_flavour                  of tag
  | `Unsupported_cmap_format          of int
  | `Unsupported_glyf_matching_points
  | `Missing_required_table           of tag
  | `Unknown_version                  of context * wint
  | `Unknown_loca_format              of context * int
  | `Unknown_composite_format         of context * int
  | `Invalid_offset                   of context * int
  | `Invalid_cp                       of int
  | `Invalid_cp_range                 of int * int
  | `Invalid_postscript_name          of string
  | `Unexpected_eoi                   of context

  | `Inconsistent_length_of_coverage  of context
  | `Inconsistent_length_of_class
  | `Invalid_lookup_order             of int
  | `Invalid_feature_index            of int
  | `Invalid_feature_params           of int
  | `Invalid_extension_position
  | `Invalid_GSUB_lookup_type         of int
  | `Invalid_GPOS_lookup_type         of int
  | `Invalid_cff_not_a_quad
  | `Invalid_cff_not_an_integer
  | `Invalid_cff_not_an_element
  | `Invalid_cff_not_an_offsize       of int
  | `Invalid_cff_not_a_singleton
  | `Missing_required_dict_long_key   of int
  | `Missing_required_dict_short_key  of int
  | `Invalid_cff_inconsistent_length
  | `Invalid_cff_invalid_first_offset
  | `Invalid_cff_no_private_dict
  | `Unknown_fdselect_format          of int
  | `Invalid_fd_select                of int
  | `Invalid_fd_index                 of int
  | `Invalid_charstring_type          of int
  | `Invalid_charstring
  | `Invalid_sid                      of int
  | `Invalid_ros
  | `Layered_ttc
  | `Invalid_index_to_loc_format      of int
  | `Invalid_mark_class               of int

  | `Not_encodable_as_uint8           of int
  | `Not_encodable_as_int8            of int
  | `Not_encodable_as_uint16          of int
  | `Not_encodable_as_int16           of int
  | `Not_encodable_as_uint24          of int
  | `Not_encodable_as_uint32          of wint
  | `Not_encodable_as_int32           of wint
  | `Not_encodable_as_time            of wint
  | `Too_many_glyphs_for_encoding     of int
  | `No_glyph_for_encoding
  | `Missing_head_table_for_encoding
]

let pp_context ppf = function
| `Table tag       -> pp ppf "table %a" Tag.pp tag
| `Offset_table    -> pp ppf "offset table"
| `Table_directory -> pp ppf "table directory"

let pp ppf = function
| `Unknown_flavour tag ->
    pp ppf "@[Unknown@ OpenType@ flavour (%a)@]" Tag.pp tag
| `Missing_required_table tag ->
    pp ppf "@[Missing@ required@ table (%a)@]" Tag.pp tag
| `Unsupported_cmap_format cmapfmt ->
    pp ppf "@[Unsupported@ cmap@ subtable@ format@ %d@]" cmapfmt
| `Unsupported_glyf_matching_points ->
    pp ppf "@[Unsupported@ glyf@ matching@ points)@]"
| `Unknown_version (ctx, v) ->
    pp ppf "@[Unknown@ version (%LX)@ in@ %a@]" (WideInt.to_int64 v) pp_context ctx
| `Unknown_loca_format (ctx, v) ->
    pp ppf "@[Unknown@ loca table format (%d)@ in@ %a@]" v pp_context ctx
| `Unknown_composite_format (ctx, v) ->
    pp ppf "@[Unknown@ composite glyph format (%d)@ in@ %a@]" v pp_context ctx
| `Invalid_offset (ctx, o) ->
    pp ppf "@[Invalid@ offset (%d)@ in@ %a@]" o pp_context ctx
| `Invalid_cp u ->
    pp ppf "@[Invalid@ Unicode@ code@ point@ (%a)@]" pp_cp u
| `Invalid_cp_range (u0, u1) ->
    pp ppf "@[Invalid@ Unicode@ code@ point@ range (%a, %a)@]" pp_cp u0 pp_cp u1
| `Invalid_postscript_name n ->
    pp ppf "@[Invalid@ PostScript@ name (%S)@]" n
| `Unexpected_eoi ctx ->
    pp ppf "@[Unexpected@ end@ of@ input@ in %a@]" pp_context ctx

| `Inconsistent_length_of_coverage ctx ->
    pp ppf "@[Inconsistent@ length@ of@ coverage@ in %a@]" pp_context ctx
| `Inconsistent_length_of_class ->
    pp ppf "@[Inconsistent@ length@ of@ class@]"
| `Missing_required_script_tag tag ->
    pp ppf "@[Missing@ required@ Script@ tag@ (%S)" tag
| `Missing_required_langsys_tag tag ->
    pp ppf "@[Missing@ required@ LangSys@ tag@ (%S)@]" tag
| `Missing_required_feature_tag tag ->
    pp ppf "@[Missing@ required@ Feature@ tag@ (%S)@]" tag
| `Invalid_lookup_order lo ->
    pp ppf "@[Invalid@ lookup@ order@ (%d)@]" lo
| `Invalid_feature_index fi ->
    pp ppf "@[Invalid@ feature@ index@ (%d)@]" fi
| `Invalid_feature_params fp ->
    pp ppf "@[Invalid@ feature@ params@ (%d)@]" fp
| `Invalid_extension_position ->
    pp ppf "@[Invalid@ extension@ position@]"
| `Invalid_GSUB_lookup_type lty ->
    pp ppf "@[Invalid@ GSUB@ LookupType@ (%d)@]" lty
| `Invalid_GPOS_lookup_type lty ->
    pp ppf "@[Invalid@ GPOS@ LookupType@ (%d)@]" lty
| `Invalid_cff_not_a_quad ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ a@ quad@]"
| `Invalid_cff_not_an_integer ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ an@ integer@]"
| `Invalid_cff_not_a_singleton ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ a@ singleton@]"
| `Invalid_cff_not_an_element ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ an@ element@]"
| `Invalid_cff_not_an_offsize(n) ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ an@ offsize@ %d@]" n
| `Missing_required_dict_short_key i ->
    pp ppf "@[Missing@ required@ key@ '%d'@ in@ a@ DICT@]" i
| `Missing_required_dict_long_key i ->
    pp ppf "@[Missing@ required@ key@ '12 %d'@ in@ a@ DICT@]" i
| `Invalid_cff_inconsistent_length ->
    pp ppf "@[Invalid@ CFF@ table;@ inconsistent@ length@]"
| `Invalid_cff_invalid_first_offset ->
    pp ppf "@[Invalid@ CFF@ table;@ invalid@ first@ offset@]"
| `Invalid_cff_no_private_dict ->
    pp ppf "@[Invalid@ CFF@ table;@ no@ Private@ DICT@]"
| `Unknown_fdselect_format n ->
    pp ppf "@[Unknown@ FDSelect@ format@ (%d)@]" n
| `Invalid_fd_select gid ->
    pp ppf "@[Invalid@ FDSelect;@ it@ lacks@ a@ necessary@ glyph@ ID (%d)@]" gid
| `Invalid_fd_index fdi ->
    pp ppf "@[Invalid@ FD@ index@ (%d)]" fdi
| `Invalid_charstring_type csty ->
    pp ppf "@[Invalid@ CharString@ type@ (%d)@]" csty
| `Invalid_charstring ->
    pp ppf "@[Invalid@ CharString@]"
| `Invalid_sid sid ->
    pp ppf "@[Invalid@ SID@ (%d)@]" sid
| `Invalid_ros ->
    pp ppf "@[Invalid@ ROS@]"
| `Layered_ttc ->
    pp ppf "@[Layered@ TTC@]"
| `Invalid_index_to_loc_format locfmt ->
    pp ppf "@[Invalid@ IndexToLocFormat@ entry@ in@ the@ 'head'@ table@ (%d)@]" locfmt
| `Invalid_mark_class i ->
    pp ppf "@[Invalid@ Mark@ class@ (%d)@]" i

| `Not_encodable_as_uint8 ui ->
    pp ppf "@[Not@ encodable@ as@ uint8@ (%d)@]" ui
| `Not_encodable_as_int8 i ->
    pp ppf "@[Not@ encodable@ as@ int8@ (%d)@]" i
| `Not_encodable_as_uint16 ui ->
    pp ppf "@[Not@ encodable@ as@ uint16@ (%d)@]" ui
| `Not_encodable_as_int16 i ->
    pp ppf "@[Not@ encodable@ as@ int16@ (%d)@]" i
| `Not_encodable_as_uint24 ui ->
    pp ppf "@[Not@ encodable@ as@ uint24@ (%d)@]" ui
| `Not_encodable_as_uint32 wui ->
    pp ppf "@[Not@ encodable@ as@ uint32@ (%a)@]" WideInt.pp wui
| `Not_encodable_as_int32 wi ->
    pp ppf "@[Not@ encodable@ as@ int32@ (%a)@]" WideInt.pp wi
| `Not_encodable_as_time wi ->
    pp ppf "@[Not@ encodable@ as@ LONGDATETIME@ (%a)@]" WideInt.pp wi
| `Too_many_glyphs_for_encoding num ->
    pp ppf "@[Too@ many@ glyphs@ for@ encoding@ (%d)@]" num
| `No_glyph_for_encoding ->
    pp ppf "@[No@ glyph@ for@ encoding@]"
| `Missing_head_table_for_encoding ->
    pp ppf "@[Missing@ 'head'@ table@ for@ encoding@]"


(* Error strings *)

let err_invalid_tag s = Printf.sprintf "invalid OpenType tag (%S)" s
