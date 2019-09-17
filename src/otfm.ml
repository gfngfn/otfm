(* -*- coding: utf-8 -*- *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli, and 2017-2019 Takashi Suwa. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open OtfTypes
open OtfCFFTypes
open OtfUtils
open OtfDecBasic

(* -- OpenType tags -- *)

module Tag = OtfTag

type tag = Tag.t

(* -- Decode -- *)

type error = OtfError.t

type 'a ok = ('a, error) result

let pp_error = OtfError.pp

let is_cp = OtfError.is_cp

type cff_decoder = OtfDecCFF.cff_decoder

type ttf_decoder = OtfDecTTF.ttf_decoder

type decoder =
  | CFF of cff_decoder
  | TTF of ttf_decoder

type ttc_element = int * common_decoder

type decoder_scheme =
  | SingleDecoder      of decoder
  | TrueTypeCollection of ttc_element list


let common (d : decoder) : common_decoder =
  match d with
  | CFF(dcff) -> OtfDecCFF.cff_common dcff
  | TTF(dttf) -> OtfDecTTF.ttf_common dttf


let d_version (type a) (singlef : decoder -> a ok) (ttcf : (unit -> (ttc_element list) ok) -> a ok) (cd : common_decoder) : a ok =
  d_uint32 cd >>= fun tagwint ->
    match OtfTag.of_wide_int tagwint with
    | t  when t = Tag.v_OTTO ->
        singlef (CFF(OtfDecCFF.make_initial_cff cd))

    | t  when t = Tag.v_true ->
        singlef (TTF(OtfDecTTF.make_initial_ttf cd))

    | t  when t = Tag.v_1_0 ->
        singlef (TTF(OtfDecTTF.make_initial_ttf cd))

    | t  when t = Tag.v_ttcf ->
        ttcf (fun () ->
          d_ttc_header_offset_list cd >>= fun offsets ->
          return (offsets |> List.map (fun offset -> (offset, cd)))
        )

    | t ->
        err (`Unknown_flavour(t))


let decoder (src : source) : decoder_scheme ok =
  let singlef d = return (SingleDecoder(d)) in
  let ttcf k =
    k () >>= fun ttcelems ->
    return (TrueTypeCollection(ttcelems))
  in
  let d = make_initial_decoder src in
  d_version singlef ttcf d


let decoder_of_ttc_element (ttcelem : ttc_element) : decoder ok =
  let singlef d = return d in
  let ttcf _ = err `Layered_ttc in
  let (offset, d) = ttcelem in
  let delem = copy_and_initialize_decoder d in
  seek_pos offset delem >>= fun () ->
  d_version singlef ttcf delem


(* TODO maybe it would be better not to maintain t_pos/i_pos,
   but rather pass them as arguments to decoding functions. *)


(* -- cmap table -- *)

let rec d_array el count i a d =
  if i = count then return a else
  el d >>= fun v ->
  a.(i) <- v;
  d_array el count (i + 1) a d

let d_cmap_4_ranges d f acc u0s u1s delta offset count =       (* ugly. *)
  let garray_pos = cur_pos d in
  let rec loop acc i =
    if i = count then return acc else
    let i' = i + 1 in
    let offset = offset.(i) in
    let delta = delta.(i) in
    let u0 = u0s.(i) in if not (is_cp u0) then err (`Invalid_cp u0) else
    let u1 = u1s.(i) in if not (is_cp u1) then err (`Invalid_cp u1) else
    if u0 > u1 then err (`Invalid_cp_range (u0, u1)) else
    if offset = 0 then begin
      (* -- The arithmetic must be performed mod 65536, this is problematic
            for Otfm's interface semantics. We need to split the range
            if the glyph range spans the bounds. -- *)
      let g0 = u0 + delta in
      let g1 = u1 + delta in
      if g0 < 0 && g1 >= 0 then
        let acc' = f acc `Glyph_range (u0, - delta - 1) (g0 land 65535) in
        loop (f acc' `Glyph_range (- delta, u1) 0) i'
      else
      if g0 <= 65535 && g1 > 65535 then
        let acc' = f acc `Glyph_range (u0, 65535 - delta) g0 in
        loop (f acc' `Glyph_range (65536 - delta, u1) 0) i'
      else (* -- glyph range is inside [0;65535] or completly outside -- *)
      loop (f acc `Glyph_range (u0, u1) (g0 land 65535)) i'
    end else begin
      let rec garray acc u u1 () =
        if u > u1 then return acc else
        d_uint16 d >>= fun gindex ->
        let g = (gindex + delta) land 65535 in
        garray (f acc `Glyph (u, u) g) (u + 1) u1 ()
      in
      let pos = garray_pos - (count - i) * 2 + offset in
      seek_pos pos d >>=
      garray acc u0 u1 >>= fun acc ->
      loop acc i'
    end
  in
  loop acc 0


let d_cmap_4 d f acc =
  (* -- now the position is set immediately AFTER the format number entry -- *)
  d_skip (2 * 2) d >>= fun () ->
  d_uint16       d >>= fun count2 ->
  let count = count2 / 2 in
  let a () = Array.make count 0 in
  d_skip (3 * 2)                  d >>= fun () ->
  d_array d_uint16 count 0 (a ()) d >>= fun u1s ->
  d_skip 2                        d >>= fun () -> (* pad *)
  d_array d_uint16 count 0 (a ()) d >>= fun u0s ->
  d_array d_int16  count 0 (a ()) d >>= fun delta ->
  d_array d_uint16 count 0 (a ()) d >>= fun offset ->
  d_cmap_4_ranges d f acc u0s u1s delta offset count


(* -- cmap Format 12: Segmented coverage
      cmap Format 13: Many-to-one range mappings -- *)

let d_cp d =
  d_uint32_int d >>= fun u ->
  if not (is_cp u) then err (`Invalid_cp(u)) else
  return u


let rec d_cmap_groups d count f kind acc =
  if count = 0 then
    return acc
  else
    d_cp d >>= fun startCharCode ->
    d_cp d >>= fun endCharCode ->
    if startCharCode > endCharCode then err (`Invalid_cp_range(startCharCode, endCharCode)) else
    d_uint32_int d >>= fun startGlyphID ->
    d_cmap_groups d (count - 1) f kind (f acc kind (startCharCode, endCharCode) startGlyphID)


let d_cmap_seg kind d f acc =
  (* -- now the position is set immediately AFTER the format number entry -- *)
  d_skip (1 * 2 + 2 * 4) d >>= fun () ->
  d_uint32_int           d >>= fun nGroups ->
  d_cmap_groups d nGroups f kind acc


let d_cmap_12 d f acc = d_cmap_seg `Glyph_range d f acc
let d_cmap_13 d f acc = d_cmap_seg `Glyph d f acc


type cmap_subtable_ids = {
  platform_id : int;
  encoding_id : int;
  format      : int;
}

type cmap_subtable = (common_decoder * int) * cmap_subtable_ids


let rec d_encoding_record offset_cmap (cd : common_decoder) : cmap_subtable ok =
  d_uint16 cd >>= fun platformID ->
  d_uint16 cd >>= fun encodingID ->
  d_fetch_long offset_cmap d_uint16 cd >>= fun (offset, format) ->
  let ids =
    {
      platform_id = platformID;
      encoding_id = encodingID;
      format      = format;
    }
  in
  return ((cd, offset), ids)


let cmap (decoder : decoder) : (cmap_subtable list) ok =
  let d = common decoder in
  init_decoder d >>= fun () ->
  seek_required_table Tag.cmap d >>= fun () ->
  let offset_cmap = cur_pos d in
  d_uint16 d >>= fun version ->                           (* -- cmap header. -- *)
  confirm (version = 0) (e_version d (!% version)) >>= fun () ->
  d_list (d_encoding_record offset_cmap) d >>= fun rawsubtbllst ->
  let subtbllst =
    rawsubtbllst |> List.filter (fun (_, ids) ->
      let pid = ids.platform_id in
      let eid = ids.encoding_id in
      let format = ids.format in
      Format.fprintf debugfmt "(pid, eid, format = %d, %d, %d)" pid eid format;  (* for debug *)
      match format with
      | (4 | 12 | 13) ->
          begin
            match (pid, eid) with
            | (0, _)   (* -- Unicode -- *)
            | (3, 1)   (* -- Windows, UCS-2 -- *)
            | (3, 10)  (* -- Windows, UCS-4 -- *)
            | (1, _)   (* -- Macintosh -- *)
                -> true

            | _ -> false
          end

      | _ -> false (* -- unsupported subtable format -- *)
    )
  in
  return subtbllst


let cmap_subtable_ids (subtbl : cmap_subtable) : cmap_subtable_ids =
  let (_, ids) = subtbl in
  ids


let cmap_subtable (subtbl : cmap_subtable) f acc =
  let ((d, offset), _) = subtbl in
  Format.fprintf debugfmt "subtable offset = %d@," offset;
  seek_pos offset d >>= fun () ->
  (* -- now the position is set at the beginning of the designated cmap subtable --  *)
  d_uint16 d >>= fun format ->
  Format.fprintf debugfmt "subtable format = %d@," format;
  match format with
  | 4  -> d_cmap_4 d f acc
  | 12 -> d_cmap_12 d f acc
  | 13 -> d_cmap_13 d f acc
  | _  -> err (`Unsupported_cmap_format(format))


(* -- glyf table -- *)


let d_rev_end_points d ccount =
  let rec loop i acc =
    if i <= 0 then
      return acc
    else
      d_uint16 d >>= fun e ->
      loop (i - 1) (e :: acc)
  in
  loop ccount []


let d_rev_flags d pt_count =
  let rec loop i acc =
    if i <= 0 then
      return acc
    else
      d_uint8 d >>= fun f ->
      if f land 8 = 0 then
        loop (i - 1) (f :: acc)
      else
        d_uint8 d >>= fun n ->
        let rec push n acc =
          if n = 0 then acc else push (n - 1) (f :: acc)
        in
        loop (i - 1 - n) (push (n + 1) acc)
  in
  loop pt_count []


let d_rev_coord short_mask same_mask d flags =
  let rec loop x acc = function
    | f :: fs ->
        if f land short_mask > 0 then
          begin
            d_uint8 d >>= fun dx ->
            let x = x + (if f land same_mask > 0 then dx else -dx) in
            loop x (x :: acc) fs
          end
        else
          begin
            if f land same_mask > 0 then
              loop x (x :: acc) fs
            else
              d_int16 d >>= fun dx ->
              let x = x + dx in
              loop x (x :: acc) fs
          end

    | [] ->
        return acc
  in
  loop 0 [] flags


let d_rev_xs d flags = d_rev_coord 2 16 d flags
let d_rev_ys d flags = d_rev_coord 4 32 d flags


let d_simple_glyph d ccount =
  if ccount = 0 then
    return []
  else
    d_rev_end_points d ccount >>= fun rev_epts ->
    let pt_count = match rev_epts with [] -> 0 | e :: _ -> e + 1 in
    d_uint16 d >>= fun ins_len ->
    d_skip ins_len d >>= fun () ->
    d_rev_flags d pt_count >>= fun rev_flags ->
    let flags = List.rev rev_flags in
    d_rev_xs d flags >>= fun rxs ->
    d_rev_ys d flags >>= fun rys ->
    let rec combine repts flags rxs rys i acc =
      match flags with
      | [] ->
          acc

      | f :: fs ->
          let (new_contour, repts) =
            match repts with
            | []                 -> (false, [])
            | e :: es when e = i -> (true, es)
            | es                 -> (false, es)
          in
          begin
            match acc with
            | c :: cs ->
                let new_pt = (f land 1 > 0,  List.hd rxs, List.hd rys) in
                let acc' =
                  if new_contour then [new_pt] :: c :: cs else
                  (new_pt :: c) :: cs
                in
                combine repts fs (List.tl rxs) (List.tl rys) (i - 1) acc'

            | _ ->
                assert false
          end
    in
    return (combine (List.tl rev_epts) rev_flags rxs rys (pt_count - 1) ([] :: []))


let d_composite_glyph d =
  let rec loop acc =
    d_uint16 d >>= fun flags ->
    d_uint16 d >>= fun gid ->
    if flags land 2 = 0 then
      err `Unsupported_glyf_matching_points
    else
      let dec = if flags land 1 > 0 then d_int16 else d_int8 in
      dec d >>= fun dx ->
      dec d >>= fun dy ->
      begin
        if flags land 8 > 0 then  (* -- scale -- *)
          d_f2dot14 d >>= fun s ->
          return (Some(s, 0., 0., s))
        else if flags land 64 > 0 then  (* -- xy scale -- *)
          d_f2dot14 d >>= fun sx ->
          d_f2dot14 d >>= fun sy ->
          return (Some(sx, 0., 0., sy))
        else if flags land 128 > 0 then  (* -- m2 -- *)
          d_f2dot14 d >>= fun a ->
          d_f2dot14 d >>= fun b ->
          d_f2dot14 d >>= fun c ->
          d_f2dot14 d >>= fun d ->
          return (Some(a, b, c, d))
        else
          return None
      end >>= fun m ->
      let accnew = Alist.extend acc (gid, (dx, dy), m) in
      if flags land 32 > 0 then
        loop accnew
      else
        return (Alist.to_list accnew)
  in
  loop Alist.empty


let glyf (dttf : ttf_decoder) (loc : glyf_loc) : glyph_descr ok =
  let open OtfDecTTF in
  let cd = ttf_common dttf in
  init_decoder cd >>= fun () ->
  init_glyf dttf >>= fun pos ->
      seek_pos (pos + loc) cd >>= fun () ->
      d_int16 cd >>= fun ccount ->
      d_int16 cd >>= fun xmin ->
      d_int16 cd >>= fun ymin ->
      d_int16 cd >>= fun xmax ->
      d_int16 cd >>= fun ymax ->
      if ccount < -1 then
        err_composite_format cd ccount
      else if ccount = -1 then
        d_composite_glyph cd >>= fun components ->
        return (Composite(components), (xmin, ymin, xmax, ymax))
      else
        d_simple_glyph cd ccount >>= fun contours ->
        return (Simple(contours), (xmin, ymin, xmax, ymax))


(* -- head table -- *)

let head (decoder : decoder) : head ok =
  let cd = common decoder in
  init_decoder cd >>= fun () ->
  seek_required_table Tag.head cd >>= fun () ->
  d_uint32 cd >>= fun version ->
  if version <> !%% 0x00010000L then err_version cd version else
  d_uint32 cd >>= fun head_font_revision ->
  d_skip 8 cd >>= fun () -> (* -- checkSumAdjustement, magicNumber -- *)
  d_uint16 cd >>= fun head_flags ->
  d_uint16 cd >>= fun head_units_per_em ->
  d_time   cd >>= fun head_created ->
  d_time   cd >>= fun head_modified ->
  d_int16  cd >>= fun head_xmin ->
  d_int16  cd >>= fun head_ymin ->
  d_int16  cd >>= fun head_xmax ->
  d_int16  cd >>= fun head_ymax ->
  d_uint16 cd >>= fun head_mac_style ->
  d_uint16 cd >>= fun head_lowest_rec_ppem ->
  d_skip 2 cd >>= fun () -> (* -- fontDirectionHint -- *)
  OtfDecTTF.d_loca_format cd >>= fun head_index_to_loc_format ->
  return {
    head_font_revision; head_flags; head_units_per_em; head_created;
    head_modified; head_xmin; head_ymin; head_xmax; head_ymax;
    head_mac_style; head_lowest_rec_ppem; head_index_to_loc_format;
  }


(* -- hhea table -- *)

let hhea (decoder : decoder) : hhea ok =
  let cd = common decoder in
  init_decoder cd >>= fun () ->
  seek_required_table Tag.hhea cd >>= fun () ->
  d_uint32 cd >>= fun version ->
  if version <> !%% 0x00010000L then err_version cd version else
  d_int16  cd >>= fun hhea_ascender ->
  d_int16  cd >>= fun hhea_descender ->
  d_int16  cd >>= fun hhea_line_gap ->
  d_uint16 cd >>= fun hhea_advance_width_max ->
  d_int16  cd >>= fun hhea_min_left_side_bearing ->
  d_int16  cd >>= fun hhea_min_right_side_bearing ->
  d_int16  cd >>= fun hhea_xmax_extent ->
  d_int16  cd >>= fun hhea_caret_slope_rise ->
  d_int16  cd >>= fun hhea_caret_slope_run ->
  d_int16  cd >>= fun hhea_caret_offset ->
  return {
    hhea_ascender; hhea_descender; hhea_line_gap; hhea_advance_width_max;
    hhea_min_left_side_bearing; hhea_min_right_side_bearing;
    hhea_xmax_extent; hhea_caret_slope_rise; hhea_caret_slope_run;
    hhea_caret_offset;
  }


(* -- hmtx table -- *)

let rec d_hmetric goffset i f acc last_adv cd =
  if i = 0 then
    return (acc, last_adv)
  else
    d_uint16 cd >>= fun adv ->
    d_int16  cd >>= fun lsb ->
    let acc' = f acc (goffset - i) adv lsb in
    d_hmetric goffset (i - 1) f acc' adv cd


let rec d_hlsb goffset i f acc adv cd =
  if i = 0 then
    return acc
  else
    d_int16 cd >>= fun lsb ->
    let acc' = f acc (goffset - i) adv lsb in
    d_hlsb goffset (i - 1) f acc' adv cd


let hmtx (decoder : decoder) f acc =
  let cd = common decoder in
  glyph_count cd >>= fun glyph_count ->
  d_hm_count  cd >>= fun hm_count ->
  seek_required_table Tag.hmtx cd >>= fun () ->
  d_hmetric hm_count hm_count f acc (-1) cd >>= fun (acc, last_adv) ->
  d_hlsb glyph_count (glyph_count - hm_count) f acc last_adv cd


(* -- maxp table -- *)

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


let maxp (decoder : decoder) : maxp ok =
  let cd = common decoder in
  init_decoder cd >>= fun () ->
  seek_required_table Tag.maxp cd >>= fun () ->
  d_uint32 cd >>= fun version ->
  confirm (version = !%% 0x00005000L || version = !%% 0x00010000L) (e_version cd version) >>= fun () ->
  d_uint16 cd >>= fun maxp_num_glyphs ->
  if version = !%% 0x00005000L then
    (* Fonts with CFF Data *)
    return {
      maxp_num_glyphs               = maxp_num_glyphs;
      maxp_max_points               = 0;
      maxp_max_contours             = 0;
      maxp_max_composite_points     = 0;
      maxp_max_composite_contours   = 0;
      maxp_max_zones                = 0;
      maxp_max_twilight_points      = 0;
      maxp_max_storage              = 0;
      maxp_max_function_defs        = 0;
      maxp_max_instruction_defs     = 0;
      maxp_max_stack_elements       = 0;
      maxp_max_size_of_instructions = 0;
      maxp_max_component_elements   = 0;
      maxp_max_component_depth      = 0;
    }
  else
    (* Fonts with TrueType outlines *)
    d_uint16 cd >>= fun maxp_max_points ->
    d_uint16 cd >>= fun maxp_max_contours ->
    d_uint16 cd >>= fun maxp_max_composite_points ->
    d_uint16 cd >>= fun maxp_max_composite_contours ->
    d_uint16 cd >>= fun maxp_max_zones ->
    d_uint16 cd >>= fun maxp_max_twilight_points ->
    d_uint16 cd >>= fun maxp_max_storage ->
    d_uint16 cd >>= fun maxp_max_function_defs ->
    d_uint16 cd >>= fun maxp_max_instruction_defs ->
    d_uint16 cd >>= fun maxp_max_stack_elements ->
    d_uint16 cd >>= fun maxp_max_size_of_instructions ->
    d_uint16 cd >>= fun maxp_max_component_elements ->
    d_uint16 cd >>= fun maxp_max_component_depth ->
    return {
      maxp_num_glyphs;
      maxp_max_points;
      maxp_max_contours;
      maxp_max_composite_points;
      maxp_max_composite_contours;
      maxp_max_zones;
      maxp_max_twilight_points;
      maxp_max_storage;
      maxp_max_function_defs;
      maxp_max_instruction_defs;
      maxp_max_stack_elements;
      maxp_max_size_of_instructions;
      maxp_max_component_elements;
      maxp_max_component_depth;
    }


(* -- name table -- *)

(* -- Source: https://skia.googlecode.com/svn/trunk/src/sfnt/SkOTTable_name.cpp
      BSD3 licensed (c) 2011 Google Inc. -- *)
let lcid_to_bcp47 = [
  0x0401, "ar-sa";  0x0402, "bg-bg";  0x0403, "ca-es";  0x0404, "zh-tw";
  0x0405, "cs-cz";  0x0406, "da-dk";  0x0407, "de-de";  0x0408, "el-gr";
  0x0409, "en-us";  0x040a, "es-es_tradnl";             0x040b, "fi-fi";
  0x040c, "fr-fr";  0x040d, "he-il";  0x040d, "he";     0x040e, "hu-hu";
  0x040e, "hu";     0x040f, "is-is";  0x0410, "it-it";  0x0411, "ja-jp";
  0x0412, "ko-kr";  0x0413, "nl-nl";  0x0414, "nb-no";  0x0415, "pl-pl";
  0x0416, "pt-br";  0x0417, "rm-ch";  0x0418, "ro-ro";  0x0419, "ru-ru";
  0x041a, "hr-hr";  0x041b, "sk-sk";  0x041c, "sq-al";  0x041d, "sv-se";
  0x041e, "th-th";  0x041f, "tr-tr";  0x0420, "ur-pk";  0x0421, "id-id";
  0x0422, "uk-ua";  0x0423, "be-by";  0x0424, "sl-si";  0x0425, "et-ee";
  0x0426, "lv-lv";  0x0427, "lt-lt";  0x0428, "tg-cyrl-tj";
  0x0429, "fa-ir";  0x042a, "vi-vn";  0x042b, "hy-am";  0x042c, "az-latn-az";
  0x042d, "eu-es";  0x042e, "hsb-de"; 0x042f, "mk-mk";  0x0432, "tn-za";
  0x0434, "xh-za";  0x0435, "zu-za";  0x0436, "af-za";  0x0437, "ka-ge";
  0x0438, "fo-fo";  0x0439, "hi-in";  0x043a, "mt-mt";  0x043b, "se-no";
  0x043e, "ms-my";  0x043f, "kk-kz";  0x0440, "ky-kg";  0x0441, "sw-ke";
  0x0442, "tk-tm";  0x0443, "uz-latn-uz";               0x0443, "uz";
  0x0444, "tt-ru";  0x0445, "bn-in";  0x0446, "pa-in";  0x0447, "gu-in";
  0x0448, "or-in";  0x0449, "ta-in";  0x044a, "te-in";  0x044b, "kn-in";
  0x044c, "ml-in";  0x044d, "as-in";  0x044e, "mr-in";  0x044f, "sa-in";
  0x0450, "mn-cyrl";0x0451, "bo-cn";  0x0452, "cy-gb";  0x0453, "km-kh";
  0x0454, "lo-la";  0x0456, "gl-es";  0x0457, "kok-in"; 0x045a, "syr-sy";
  0x045b, "si-lk";  0x045d, "iu-cans-ca";               0x045e, "am-et";
  0x0461, "ne-np";  0x0462, "fy-nl";  0x0463, "ps-af";  0x0464, "fil-ph";
  0x0465, "dv-mv";  0x0468, "ha-latn-ng";               0x046a, "yo-ng";
  0x046b, "quz-bo"; 0x046c, "nso-za"; 0x046d, "ba-ru";  0x046e, "lb-lu";
  0x046f, "kl-gl";  0x0470, "ig-ng";  0x0478, "ii-cn";  0x047a, "arn-cl";
  0x047c, "moh-ca"; 0x047e, "br-fr";  0x0480, "ug-cn";  0x0481, "mi-nz";
  0x0482, "oc-fr";  0x0483, "co-fr";  0x0484, "gsw-fr"; 0x0485, "sah-ru";
  0x0486, "qut-gt"; 0x0487, "rw-rw";  0x0488, "wo-sn";  0x048c, "prs-af";
  0x0491, "gd-gb";  0x0801, "ar-iq";  0x0804, "zh-hans";0x0807, "de-ch";
  0x0809, "en-gb";  0x080a, "es-mx";  0x080c, "fr-be";  0x0810, "it-ch";
  0x0813, "nl-be";  0x0814, "nn-no";  0x0816, "pt-pt";  0x081a, "sr-latn-cs";
  0x081d, "sv-fi";  0x082c, "az-cyrl-az";               0x082e, "dsb-de";
  0x082e, "dsb";    0x083b, "se-se";  0x083c, "ga-ie";  0x083e, "ms-bn";
  0x0843, "uz-cyrl-uz";               0x0845, "bn-bd";  0x0850, "mn-mong-cn";
  0x085d, "iu-latn-ca";               0x085f, "tzm-latn-dz";
  0x086b, "quz-ec"; 0x0c01, "ar-eg";  0x0c04, "zh-hant";0x0c07, "de-at";
  0x0c09, "en-au";  0x0c0a, "es-es";  0x0c0c, "fr-ca";  0x0c1a, "sr-cyrl-cs";
  0x0c3b, "se-fi";  0x0c6b, "quz-pe"; 0x1001, "ar-ly";  0x1004, "zh-sg";
  0x1007, "de-lu";  0x1009, "en-ca";  0x100a, "es-gt";  0x100c, "fr-ch";
  0x101a, "hr-ba";  0x103b, "smj-no"; 0x1401, "ar-dz";  0x1404, "zh-mo";
  0x1407, "de-li";  0x1409, "en-nz";  0x140a, "es-cr";  0x140c, "fr-lu";
  0x141a, "bs-latn-ba";               0x141a, "bs";     0x143b, "smj-se";
  0x143b, "smj";    0x1801, "ar-ma";  0x1809, "en-ie";  0x180a, "es-pa";
  0x180c, "fr-mc";  0x181a, "sr-latn-ba";               0x183b, "sma-no";
  0x1c01, "ar-tn";  0x1c09, "en-za";  0x1c0a, "es-do";  0x1c1a, "sr-cyrl-ba";
  0x1c3b, "sma-se"; 0x1c3b, "sma";    0x2001, "ar-om";  0x2009, "en-jm";
  0x200a, "es-ve";  0x201a, "bs-cyrl-ba";               0x201a, "bs-cyrl";
  0x203b, "sms-fi"; 0x203b, "sms";    0x2401, "ar-ye";  0x2409, "en-029";
  0x240a, "es-co";  0x241a, "sr-latn-rs";               0x243b, "smn-fi";
  0x2801, "ar-sy";  0x2809, "en-bz";  0x280a, "es-pe";  0x281a, "sr-cyrl-rs";
  0x2c01, "ar-jo";  0x2c09, "en-tt";  0x2c0a, "es-ar";  0x2c1a, "sr-latn-me";
  0x3001, "ar-lb";  0x3009, "en-zw";  0x300a, "es-ec";  0x301a, "sr-cyrl-me";
  0x3401, "ar-kw";  0x3409, "en-ph";  0x340a, "es-cl";  0x3801, "ar-ae";
  0x380a, "es-uy";  0x3c01, "ar-bh";  0x3c0a, "es-py";  0x4001, "ar-qa";
  0x4009, "en-in";  0x400a, "es-bo";  0x4409, "en-my";  0x440a, "es-sv";
  0x4809, "en-sg";  0x480a, "es-hn";  0x4c0a, "es-ni";  0x500a, "es-pr";
  0x540a, "es-us"; ]


type lang = string


let rec d_name_langs pos_name soff ncount d =
  d_skip (ncount * 6 * 2) d >>= fun () ->
  d_uint16                d >>= fun lcount ->
  let rec loop i acc =
    if ncount = 0 then return acc else
    d_uint16 d >>= fun len ->
    d_uint16 d >>= fun off ->
    let cpos = cur_pos d in
    seek_pos (pos_name + soff + off) d >>= fun () ->
    d_utf_16be len d >>= fun lang ->
    seek_pos cpos d >>= fun () ->
    loop (i - 1) ((0x8000 + (ncount - i), lang) :: acc)
  in
  loop ncount []


let rec d_name_records pos_name soff ncount f acc langs seen d =
  let d_iter = d_name_records pos_name soff in
  if ncount = 0 then return acc else
  d_uint16 d >>= fun pid ->
  d_uint16 d >>= fun eid ->
  d_uint16 d >>= fun lid ->
  d_uint16 d >>= fun nid ->
  d_uint16 d >>= fun len ->
  d_uint16 d >>= fun off ->
  match (pid, eid) with
  | ((0 | 2), _) | (3, 1) ->
      let cpos = cur_pos d in
      let n = (nid, lid) in
      if List.mem n seen then
        d_iter (ncount - 1) f acc langs seen d
      else
        seek_pos (pos_name + soff + off) d >>= fun () ->
        d_utf_16be len d >>= fun v ->
        seek_pos cpos  d >>= fun () ->
        let lang = try List.assoc lid langs with Not_found -> "und" in
        let acc' = f acc nid lang v in
        d_iter (ncount - 1) f acc' langs (n :: seen) d

  | _ ->
      d_iter (ncount - 1) f acc langs seen d


let name (decoder : decoder) f acc =
  let cd = common decoder in
  init_decoder cd >>= fun () ->
  seek_required_table Tag.name cd >>= fun () ->
  let pos_name = cur_pos cd in
  d_uint16 cd >>= fun version ->
  confirm (0 <= version && version <= 1) (e_version cd (!% version)) >>= fun () ->
  d_uint16 cd >>= fun ncount ->
  d_uint16 cd >>= fun soff ->
  let cpos = cur_pos cd in
  (if version = 0 then return [] else d_name_langs pos_name soff ncount cd) >>= fun langs ->
  let langs = List.rev_append langs lcid_to_bcp47 in
  seek_pos cpos cd >>= fun () ->
  d_name_records pos_name soff ncount f acc langs [] cd


(* -- OS/2 table -- *)

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
  os2_panose                 : string;  (* -- 10 bytes -- *)
  os2_ul_unicode_range1      : wint;
  os2_ul_unicode_range2      : wint;
  os2_ul_unicode_range3      : wint;
  os2_ul_unicode_range4      : wint;
  os2_ach_vend_id            : wint;
  os2_fs_selection           : int;
  os2_us_first_char_index    : int;
  os2_us_last_char_index     : int;
  os2_s_typo_ascender        : int;
  os2_s_type_descender       : int;
  os2_s_typo_linegap         : int;
  os2_us_win_ascent          : int;
  os2_us_win_descent         : int;
  os2_ul_code_page_range_1   : wint option;
  os2_ul_code_page_range_2   : wint option;
  os2_s_x_height             : int option;
  os2_s_cap_height           : int option;
  os2_us_default_char        : int option;
  os2_us_break_char          : int option;
  os2_us_max_context         : int option;
}

let os2 (decoder : decoder) : os2 ok =
  let cd = common decoder in
  init_decoder cd >>= fun () ->
  seek_required_table Tag.os2 cd >>= fun () ->
  d_uint16 cd >>= fun version ->
  confirm (version <= 0x0004) (e_version cd (!% version)) >>= fun () ->
  let opt v cdf cd =
    if version < v then return None else cdf cd >>= fun v -> return (Some(v))
  in
  d_int16  cd >>= fun os2_x_avg_char_width ->
  d_uint16 cd >>= fun os2_us_weight_class ->
  d_uint16 cd >>= fun os2_us_width_class ->
  d_uint16 cd >>= fun os2_fs_type ->
  d_int16  cd >>= fun os2_y_subscript_x_size ->
  d_int16  cd >>= fun os2_y_subscript_y_size ->
  d_int16  cd >>= fun os2_y_subscript_x_offset ->
  d_int16  cd >>= fun os2_y_subscript_y_offset ->
  d_int16  cd >>= fun os2_y_superscript_x_size ->
  d_int16  cd >>= fun os2_y_superscript_y_size ->
  d_int16  cd >>= fun os2_y_superscript_x_offset ->
  d_int16  cd >>= fun os2_y_superscript_y_offset ->
  d_int16  cd >>= fun os2_y_strikeout_size ->
  d_int16  cd >>= fun os2_y_strikeout_position ->
  d_int16  cd >>= fun os2_family_class ->
  d_bytes 10 cd >>= fun os2_panose ->
  d_uint32 cd >>= fun os2_ul_unicode_range1 ->
  d_uint32 cd >>= fun os2_ul_unicode_range2 ->
  d_uint32 cd >>= fun os2_ul_unicode_range3 ->
  d_uint32 cd >>= fun os2_ul_unicode_range4 ->
  d_uint32 cd >>= fun os2_ach_vend_id ->
  d_uint16 cd >>= fun os2_fs_selection ->
  d_uint16 cd >>= fun os2_us_first_char_index ->
  d_uint16 cd >>= fun os2_us_last_char_index ->
  d_int16  cd >>= fun os2_s_typo_ascender ->
  d_int16  cd >>= fun os2_s_type_descender ->
  d_int16  cd >>= fun os2_s_typo_linegap ->
  d_uint16 cd >>= fun os2_us_win_ascent ->
  d_uint16 cd >>= fun os2_us_win_descent ->
  opt 0x0001 d_uint32 cd >>= fun os2_ul_code_page_range_1 ->
  opt 0x0001 d_uint32 cd >>= fun os2_ul_code_page_range_2 ->
  opt 0x0002 d_int16  cd >>= fun os2_s_x_height ->
  opt 0x0002 d_int16  cd >>= fun os2_s_cap_height ->
  opt 0x0002 d_uint16 cd >>= fun os2_us_default_char ->
  opt 0x0002 d_uint16 cd >>= fun os2_us_break_char ->
  opt 0x0002 d_uint16 cd >>= fun os2_us_max_context ->
  return {
    os2_x_avg_char_width; os2_us_weight_class; os2_us_width_class;
    os2_fs_type; os2_y_subscript_x_size; os2_y_subscript_y_size;
    os2_y_subscript_x_offset; os2_y_subscript_y_offset;
    os2_y_superscript_x_size; os2_y_superscript_y_size;
    os2_y_superscript_x_offset; os2_y_superscript_y_offset;
    os2_y_strikeout_size; os2_y_strikeout_position;
    os2_family_class; os2_panose; os2_ul_unicode_range1;
    os2_ul_unicode_range2; os2_ul_unicode_range3;
    os2_ul_unicode_range4; os2_ach_vend_id; os2_fs_selection;
    os2_us_first_char_index; os2_us_last_char_index;
    os2_s_typo_ascender; os2_s_type_descender; os2_s_typo_linegap;
    os2_us_win_ascent; os2_us_win_descent;
    os2_ul_code_page_range_1; os2_ul_code_page_range_2;
    os2_s_x_height; os2_s_cap_height; os2_us_default_char;
    os2_us_break_char; os2_us_max_context;
  }


(* -- kern table -- *)

type kern_info ={
  kern_dir          : [ `H | `V ];
  kern_kind         : [ `Min | `Kern ];
  kern_cross_stream : bool;
}


let kern_info c =
  {
    kern_dir = (if c land 0x1 > 0 then `H else `V);
    kern_kind = (if c land 0x2 > 0 then `Min else `Kern);
    kern_cross_stream = c land 0x4 > 0;
  }


let rec kern_tables ntables t p acc d =
  if ntables = 0 then
    return acc
  else
    d_uint16 d >>= fun version ->
    confirm (version = 0) (e_version d (!% version)) >>= fun () ->
    d_uint16 d >>= fun len ->
    d_uint16 d >>= fun coverage ->
    let format = coverage lsr 8 in
    let skip acc =
      d_skip (len - 3 * 2) d >>= fun () ->
      kern_tables (ntables - 1) t p acc d
    in
    if format <> 0 then
      skip acc
    else
      match t acc (kern_info coverage) with
      | (`Skip, acc) ->
          skip acc

      | (`Fold, acc) ->
          let rec d_pairs len acc d =
            if len < 3 * 2 then
              d_skip len d >>= fun () ->
              return acc
            else
              d_uint16 d >>= fun left ->
              d_uint16 d >>= fun right ->
              d_int16 d >>= fun values ->
              d_pairs (len - 3 * 2) (p acc left right values) d
          in
          d_skip (4 * 2)  d >>= fun () ->
          d_pairs len acc d >>= fun acc ->
          kern_tables (ntables - 1) t p acc d


let kern (decoder : decoder) t p acc =
  let d = common decoder in
  init_decoder d >>= fun () ->
  seek_table Tag.kern d >>= function
  | None ->
      return acc

  | Some(_) ->
      d_uint16 d >>= fun version ->
      confirm (version = 0) (e_version d (!% version)) >>= fun () ->
      d_uint16 d >>= fun ntables ->
      kern_tables ntables t p acc d


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


(* -- GSUB table -- *)

type gsub_subtable =
  | SingleSubtable    of (glyph_id * glyph_id) list
      (* -- LookupType 1: Single substitution subtable [page 251] -- *)
  | AlternateSubtable of (glyph_id * (glyph_id list)) list
      (* -- LookupType 3: Alternate substitution subtable [page 253] -- *)
  | LigatureSubtable  of (glyph_id * (glyph_id list * glyph_id) list) list
      (* -- LookupType 4: Ligature substitution subtable [page 254] -- *)
  | UnsupportedGSUBSubtable
  (* temporary; should contain more lookup type *)


let d_range_record d =
  let rec range acc i j =
    if i > j then Alist.to_list acc else
      range (Alist.extend acc i) (i + 1) j
  in
  d_uint16 d >>= fun start_gid ->
  d_uint16 d >>= fun end_gid ->
  d_uint16 d >>= fun _ -> (* -- startCoverageIndex; can be ignored -- *)
  return (range Alist.empty start_gid end_gid)


let d_coverage d : (glyph_id list) ok =
    (* -- the position is supposed to be set
          to the beginning of a Coverage table [page 139] -- *)
  d_uint16 d >>= fun coverageFormat ->
  match coverageFormat with
  | 1 -> d_list d_uint16 d
  | 2 -> d_list d_range_record d >>= fun rnglst -> return (List.concat rnglst)
  | _ -> err_version d (!% coverageFormat)


let combine_coverage d coverage lst =
  try return (List.combine coverage lst) with
  | Invalid_argument(_) -> err_coverage_length d


let seek_every_pos (type a) (offsetlst : int list) (cdf : common_decoder -> a ok) (cd : common_decoder) : (a list) ok =
  let rec aux acc offsetlst =
  match offsetlst with
  | []             -> return (Alist.to_list acc)
  | offset :: tail ->
      seek_pos offset cd >>= fun () ->
      cdf cd >>= fun data ->
      aux (Alist.extend acc data) tail
  in
    aux Alist.empty offsetlst


let d_with_coverage (type a) (offset_Substitution_table : int) (cdf : common_decoder -> a ok) (cd : common_decoder) : ((glyph_id * a) list) ok =
    (* -- the position is supposed to be set
          just before a Coverage field and a subsequent offset list
          [page 254 etc.] -- *)
  d_fetch offset_Substitution_table d_coverage cd >>= fun coverage ->

    (* -- the position is set just before LigSetCount field [page 254] -- *)
  d_list (d_fetch offset_Substitution_table cdf) cd >>= fun datalst ->
  combine_coverage cd coverage datalst


type gxxx_script = {
  script_decoder             : common_decoder;
  script_tag                 : string;
  script_offset_Script_table : int;
  script_offset_FeatureList  : int;
  script_offset_LookupList   : int;
}

type gsub_script = gxxx_script

type gpos_script = gxxx_script


let gxxx_script_tag script = script.script_tag

let gsub_script_tag = gxxx_script_tag

let gpos_script_tag = gxxx_script_tag


let d_tag_offset_record offset_Gxxx d =
  d_bytes 4 d >>= fun tag ->
  d_offset offset_Gxxx d >>= fun offset ->
  return (tag, offset)


let d_tag_offset_list d : ((string * int) list) ok =
  let offset_ScriptList = cur_pos d in
  d_list (d_tag_offset_record offset_ScriptList) d


let gxxx_script tag_Gxxx (decoder : decoder) : ((gxxx_script list) option) ok =
  let d = common decoder in
  init_decoder d >>= fun () ->
  seek_table tag_Gxxx d >>= function
  | None ->
      return None

  | Some(_) ->
      let offset_Gxxx = cur_pos d in
      d_uint32 d >>= fun version ->
      confirm (version = !%% 0x00010000L) (e_version d version) >>= fun () ->
      d_fetch offset_Gxxx d_tag_offset_list d >>= fun scriptList ->
      d_offset offset_Gxxx d >>= fun offset_FeatureList ->
      d_offset offset_Gxxx d >>= fun offset_LookupList ->
      let scriptList_res =
        scriptList |> List.map (fun (scriptTag, offset_Script_table) ->
          {
            script_decoder             = d;
            script_tag                 = scriptTag;
            script_offset_Script_table = offset_Script_table;
            script_offset_FeatureList  = offset_FeatureList;
            script_offset_LookupList   = offset_LookupList;
          }
        )
      in
      return (Some(scriptList_res))


let gsub_script = gxxx_script Tag.gsub

let gpos_script = gxxx_script Tag.gpos


type gxxx_langsys = {
  langsys_decoder            : common_decoder;
  langsys_tag                : string;
  langsys_offset_LangSys     : int;
  langsys_offset_FeatureList : int;
  langsys_offset_LookupList  : int;
}

type gsub_langsys = gxxx_langsys

type gpos_langsys = gxxx_langsys


let gxxx_langsys_tag langsys = langsys.langsys_tag

let gsub_langsys_tag = gxxx_langsys_tag

let gpos_langsys_tag = gxxx_langsys_tag


let gxxx_langsys (script : gxxx_script) : (gxxx_langsys option * gxxx_langsys list) ok =
  let d = script.script_decoder in
  let offset_Script_table = script.script_offset_Script_table in
  let offset_FeatureList = script.script_offset_FeatureList in
  let offset_LookupList = script.script_offset_LookupList in
  Format.fprintf fmtGSUB "offset_Script_table = %d@," offset_Script_table;  (* for debug *)
  seek_pos offset_Script_table d >>= fun () ->
  d_offset_opt offset_Script_table d >>= fun offset_DefaultLangSys_opt ->
  d_list (d_tag_offset_record offset_Script_table) d >>= fun langSysList ->
  let defaultLangSys_res =
    match offset_DefaultLangSys_opt with
    | None ->
        None

    | Some(offset_DefaultLangSys) ->
        Some{
          langsys_decoder            = d;
          langsys_tag                = "DFLT";
          langsys_offset_LangSys     = offset_DefaultLangSys;
          langsys_offset_FeatureList = offset_FeatureList;
          langsys_offset_LookupList  = offset_LookupList;
        }
  in
  let langSysList_res =
    langSysList |> List.map (fun (langSysTag, offset_LangSys_table) ->
      {
        langsys_decoder            = d;
        langsys_tag                = langSysTag;
        langsys_offset_LangSys     = offset_LangSys_table;
        langsys_offset_FeatureList = offset_FeatureList;
        langsys_offset_LookupList  = offset_LookupList;
      }
    )
  in
  return (defaultLangSys_res, langSysList_res)


let gsub_langsys = gxxx_langsys

let gpos_langsys = gxxx_langsys


type gxxx_feature = {
  feature_decoder           : common_decoder;
  feature_tag               : string;
  feature_offset_Feature    : int;
  feature_offset_LookupList : int;
}

type gsub_feature = gxxx_feature

type gpos_feature = gxxx_feature


let gxxx_feature_tag feature = feature.feature_tag

let gsub_feature_tag = gxxx_feature_tag

let gpos_feature_tag = gxxx_feature_tag


let gxxx_feature (langsys : gxxx_langsys) : (gxxx_feature option * gxxx_feature list) ok =
  let d = langsys.langsys_decoder in
  let offset_LangSys_table = langsys.langsys_offset_LangSys in
  let offset_FeatureList = langsys.langsys_offset_FeatureList in
  let offset_LookupList = langsys.langsys_offset_LookupList in

  Format.fprintf fmtGSUB "offset_LangSys_table = %d@," offset_LangSys_table;

  seek_pos offset_LangSys_table d >>= fun () ->
    (* -- now the position is set to the beginning of the required LangSys table *)
  d_uint16 d >>= fun offset_LookupOrder ->
  confirm (offset_LookupOrder = 0) (`Invalid_lookup_order(offset_LookupOrder)) >>= fun () ->
  d_uint16 d >>= fun requiredFeatureIndex ->
  d_list d_uint16 d >>= fun featureIndex_list ->
    (* -- now we are going to see FeatureList table -- *)
  Format.fprintf fmtGSUB "offset_FeatureList = %d@," offset_FeatureList;  (* for debug *)
  seek_pos offset_FeatureList d >>= fun () ->
  d_list_filtered (d_tag_offset_record offset_FeatureList) (fun fi -> List.mem fi featureIndex_list) d >>= fun featureList ->
  let featureList_res =
    featureList |> List.map (fun (featureTag, offset_Feature_table) ->
      {
        feature_decoder = d;
        feature_tag = featureTag;
        feature_offset_Feature = offset_Feature_table;
        feature_offset_LookupList = offset_LookupList;
      }
    )
  in
  let featurereq =
        match requiredFeatureIndex with
        | 0xFFFF ->
            return None

        | _ ->
            begin
              seek_pos offset_FeatureList d >>= fun () ->
              d_list_access (d_tag_offset_record offset_FeatureList) requiredFeatureIndex d >>= function
              | None           -> err (`Invalid_feature_index(requiredFeatureIndex))
              | Some(_) as opt -> return opt
            end
  in
  featurereq >>= function
  | None ->
      return (None, featureList_res)

  | Some((tagreq, offsetreq)) ->
      let featurereq =
        {
          feature_decoder = d;
          feature_tag = tagreq;
          feature_offset_Feature = offsetreq;
          feature_offset_LookupList = offset_LookupList;
        }
      in
      return (Some(featurereq), featureList_res)


let gsub_feature = gxxx_feature

let gpos_feature = gxxx_feature


let gxxx_subtable_list (type a) (lookup_gxxx : common_decoder -> a ok) (feature : gxxx_feature) : (a list) ok =
  let d = feature.feature_decoder in
  let offset_Feature_table = feature.feature_offset_Feature in
  let offset_LookupList = feature.feature_offset_LookupList in
  Format.fprintf fmtGSUB "offset_Feature_table = %d@," offset_Feature_table;  (* for debug *)
  seek_pos offset_Feature_table d >>= fun () ->
    (* -- now the position is set to the beginning of the required Feature table -- *)
  Format.fprintf fmtGSUB "---- Feature table ----@,";  (* for debug *)
  d_uint16 d >>= fun featureParams ->
(*
  confirm (featureParams = 0) (`Invalid_feature_params(featureParams)) >>= fun () ->
*)
  d_list d_uint16 d >>= fun lookupListIndexList ->
  Format.fprintf fmtGSUB "offset_LookupList = %d@," offset_LookupList;  (* for debug *)
  seek_pos offset_LookupList d >>= fun () ->
    (* -- now the position is set to the beginning of the LookupList table -- *)
  d_list_filtered (d_offset offset_LookupList) (fun l -> List.mem l lookupListIndexList) d >>= fun offsetlst_Lookup_table ->
  seek_every_pos offsetlst_Lookup_table lookup_gxxx d


let d_ligature_table d : (glyph_id list * glyph_id) ok =
    (* -- the position is supposed to be set
          to the beginning of a Ligature table [page 255] -- *)
  d_uint16 d >>= fun ligGlyph ->
  d_uint16 d >>= fun compCount ->
  Format.fprintf fmtGSUB "    [ligGlyph = %d ----> compCount = %d]@," ligGlyph compCount;
  d_repeat (compCount - 1) d_uint16 d >>= fun component ->
  return (component, ligGlyph)


let d_ligature_set_table d : ((glyph_id list * glyph_id) list) ok =
    (* -- the position is supposed to be set
          to the beginning of a LigatureSet table [page 254] -- *)
  let offset_LigatureSet_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_LigatureSet_table = %d@," offset_LigatureSet_table;
  d_list (d_fetch offset_LigatureSet_table d_ligature_table) d


let d_ligature_substitution_subtable d : ((glyph_id * (glyph_id list * glyph_id) list) list) ok =
    (* -- the position is supposed to be set
          to the beginning of Ligature SubstFormat1 subtable [page 254] -- *)
  let offset_Substitution_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_Substitution_table = %d@," offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  Format.fprintf fmtGSUB "substFormat = %d@," substFormat;  (* for debug *)
  confirm (substFormat = 1) (e_version d (!% substFormat)) >>= fun () ->
  d_with_coverage offset_Substitution_table d_ligature_set_table d


let d_alternate_set_table d : (glyph_id list) ok =
    (* -- the position is supposed to be set
       to the beginning of AlternateSet table [page 253] -- *)
  d_list d_uint16 d


let d_alternate_substitution_subtable d : ((glyph_id * glyph_id list) list) ok =
    (* -- the position is supposed to be set
       to the beginning of Alternate SubstFormat1 subtable [page 253] -- *)
  let offset_Substitution_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_Substitution_table = %d@," offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  confirm (substFormat = 1) (e_version d (!% substFormat)) >>= fun () ->
  d_with_coverage offset_Substitution_table d_alternate_set_table d


let d_single_substitution_subtable_format_1 offset_Substitution_table d =
  d_fetch offset_Substitution_table d_coverage d >>= fun coverage ->
  d_uint16 d >>= fun deltaGlyphID ->
  return (coverage |> List.map (fun gid -> (gid, gid + deltaGlyphID)))


let d_single_substitution_subtable_format_2 offset_Substitution_table d =
  d_with_coverage offset_Substitution_table d_uint16 d


let d_single_substitution_subtable d : ((glyph_id * glyph_id) list) ok =
    (* -- the position is supposed to be set
       to the beginning of Single SubstFormat1 or Single SubstFormat2 subtable [page 251] -- *)
  let offset_Substitution_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_Substitution_table = %d@," offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  match substFormat with
  | 1 -> d_single_substitution_subtable_format_1 offset_Substitution_table d
  | 2 -> d_single_substitution_subtable_format_2 offset_Substitution_table d
  | _ -> err_version d (!% substFormat)


let lookup_gsub d : gsub_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of a Lookup table [page 137] -- *)
  let offset_Lookup_table = cur_pos d in
  d_uint16 d >>= fun lookupType ->
  Format.fprintf fmtGSUB "# lookupType = %d@," lookupType;  (* for debug *)
  d_uint16 d >>= fun lookupFlag ->
  Format.fprintf fmtGSUB "# lookupFlag = %d@," lookupFlag;  (* for debug *)
(*
  let rightToLeft      = 0 < lookupFlag land 1 in
  let ignoreBaseGlyphs = 0 < lookupFlag land 2 in
  let ignoreLigatures  = 0 < lookupFlag land 4 in
  let ignoreMarks      = 0 < lookupFlag land 8 in
*)
  match lookupType with
  | 1 ->  (* -- single substitution -- *)
      Format.fprintf fmtGSUB "LookupType 1@,";
      d_list (d_fetch offset_Lookup_table d_single_substitution_subtable) d >>= fun gid_single_assoc_list ->
      return (SingleSubtable(List.concat gid_single_assoc_list))

  | 2 ->  (* -- multiple substitution -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 3 ->  (* -- alternate substitution -- *)
      Format.fprintf fmtGSUB "LookupType 3@,";
      d_list (d_fetch offset_Lookup_table d_alternate_substitution_subtable) d >>= fun gid_altset_assoc_list ->
      return (AlternateSubtable(List.concat gid_altset_assoc_list))

  | 4 ->  (* -- ligature substitution -- *)
      Format.fprintf fmtGSUB "LookupType 4@,";  (* for debug *)
      d_list (d_fetch offset_Lookup_table d_ligature_substitution_subtable) d >>= fun gidfst_ligset_assoc_list ->
      return (LigatureSubtable(List.concat gidfst_ligset_assoc_list))

  | 5 ->  (* -- contextual substitution subtable -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 6 ->  (* -- chaining contextual substitution subtable -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 7 ->  (* -- extension substitution -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 8 ->  (* -- reverse chaining contextual single substitution subtable -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | _ ->
      err (`Invalid_GSUB_lookup_type(lookupType))


type 'a folding_gsub_single = 'a -> glyph_id * glyph_id -> 'a

type 'a folding_gsub_alt = 'a -> glyph_id * glyph_id list -> 'a

type 'a folding_gsub_lig = 'a -> glyph_id * (glyph_id list * glyph_id) list -> 'a


let rec fold_subtables_gsub
    (f_single : 'a folding_gsub_single)
    (f_alt : 'a folding_gsub_alt)
    (f_lig : 'a folding_gsub_lig)
    (init : 'a) (subtablelst : gsub_subtable list) : 'a =
  let iter = fold_subtables_gsub f_single f_alt f_lig in
    match subtablelst with
    | [] ->
        init

    | SingleSubtable(gid_single_assoc) :: tail ->
        let initnew = List.fold_left f_single init gid_single_assoc in
          iter initnew tail

    | AlternateSubtable(gid_altset_assoc) :: tail ->
        let initnew = List.fold_left f_alt init gid_altset_assoc in
          iter initnew tail

    | LigatureSubtable(gidfst_ligset_assoc) :: tail ->
        let initnew = List.fold_left f_lig init gidfst_ligset_assoc in
          iter initnew tail

    | UnsupportedGSUBSubtable :: tail ->
          iter init tail


let gsub feature
    ?single:(f_single = (fun x _ -> x))
    ?alt:(f_alt = (fun x _ -> x))
    ?lig:(f_lig = (fun x _ -> x))
    init =
  gxxx_subtable_list lookup_gsub feature >>= fun subtablelst ->
  return (fold_subtables_gsub f_single f_alt f_lig init subtablelst)


let d_if cond df d =
  if cond then
    df d >>= fun res ->
    return (Some(res))
  else
    return None


type value_format = ValueFormat of int

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


let d_value_format d : value_format ok =
  d_uint16 d >>= fun raw ->
  Ok(ValueFormat(raw))


let d_value_record (ValueFormat(valfmt)) d : value_record ok =
    (* -- the position is supposed to be set
          to the beginning of a ValueRecord table [page 213] -- *)
  d_if (0 < valfmt land   1) d_int16 d >>= fun xPlacement_opt ->
  d_if (0 < valfmt land   2) d_int16 d >>= fun yPlacement_opt ->
  d_if (0 < valfmt land   4) d_int16 d >>= fun xAdvance_opt ->
  d_if (0 < valfmt land   8) d_int16 d >>= fun yAdvance_opt ->
  d_if (0 < valfmt land  16) d_int16 d >>= fun xPlaDevice_opt ->
  d_if (0 < valfmt land  32) d_int16 d >>= fun yPlaDevice_opt ->
  d_if (0 < valfmt land  64) d_int16 d >>= fun xAdvDevice_opt ->
  d_if (0 < valfmt land 128) d_int16 d >>= fun yAdvDevice_opt ->
  return {
    x_placement  = xPlacement_opt;
    y_placement  = yPlacement_opt;
    x_advance    = xAdvance_opt;
    y_advance    = yAdvance_opt;
    x_pla_device = xPlaDevice_opt;
    y_pla_device = yPlaDevice_opt;
    x_adv_device = xAdvDevice_opt;
    y_adv_device = yAdvDevice_opt;
  }

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

type gpos_subtable =
  | SinglePosAdjustment1 of glyph_id list * value_record
  | SinglePosAdjustment2 of (glyph_id * value_record) list
  | PairPosAdjustment1   of (glyph_id * (glyph_id * value_record * value_record) list) list
  | PairPosAdjustment2   of class_definition list * class_definition list * (class_value * (class_value * value_record * value_record) list) list
  | MarkBasePos1         of int * (glyph_id * mark_record) list * (glyph_id * base_record) list
  | MarkLigPos1          of int * (glyph_id * mark_record) list * (glyph_id * ligature_attach) list
  | MarkMarkPos1         of int * (glyph_id * mark_record) list * (glyph_id * mark2_record) list
  (* temporary; must contain more kinds of adjustment subtables *)


let d_class = d_uint16


let d_pair_value_record valfmt1 valfmt2 d : (glyph_id * value_record * value_record) ok =
  d_uint16 d >>= fun secondGlyph ->
  d_value_record valfmt1 d >>= fun value1 ->
  d_value_record valfmt2 d >>= fun value2 ->
  return (secondGlyph, value1, value2)

let d_pair_set valfmt1 valfmt2 d : ((glyph_id * value_record * value_record) list) ok =
  d_list (d_pair_value_record valfmt1 valfmt2) d


let d_class_2_record valfmt1 valfmt2 d : (value_record * value_record) ok =
  d_value_record valfmt1 d >>= fun valrcd1 ->
  d_value_record valfmt2 d >>= fun valrcd2 ->
  return (valrcd1, valrcd2)


let numbering lst =
  let rec aux acc i lst =
    match lst with
    | []           -> Alist.to_list acc
    | head :: tail -> aux (Alist.extend acc (i, head)) (i + 1) tail
  in
    aux Alist.empty 0 lst


let d_class_1_record class2Count valfmt1 valfmt2 d : ((class_value * value_record * value_record) list) ok =
  d_repeat class2Count (d_class_2_record valfmt1 valfmt2) d >>= fun pairlst ->
  try return (numbering pairlst |> List.map (fun (x, (y, z)) -> (x, y, z))) with
  | Invalid_argument(_) -> err `Inconsistent_length_of_class


let d_class_definition_format_1 d : (class_definition list) ok =
  let rec aux acc gidstt lst =
    match lst with
    | []          -> return (Alist.to_list acc)
    | cls :: tail -> aux (Alist.extend acc (GlyphToClass(gidstt, cls))) (gidstt + 1) tail
  in
    d_uint16 d >>= fun startGlyph ->
    d_list d_class d >>= fun classValueArray ->
    aux Alist.empty startGlyph classValueArray


let d_class_range_record d : class_definition ok =
    d_uint16 d >>= fun start_gid ->
    d_uint16 d >>= fun end_gid ->
    d_class d >>= fun cls ->
    return (GlyphRangeToClass(start_gid, end_gid, cls))


let d_class_definition_format_2 d : (class_definition list) ok =
  d_list d_class_range_record d >>= fun rangelst ->
  return rangelst


let d_class_definition d : (class_definition list) ok =
    (* -- the position is supposed to be set
          to the  beginning of a ClassDef table [page 140] -- *)
  d_uint16 d >>= fun classFormat ->
  match classFormat with
  | 1 -> d_class_definition_format_1 d
  | 2 -> d_class_definition_format_2 d
  | _ -> err_version d (!% classFormat)


let d_single_adjustment_subtable d : gpos_subtable ok =
    (* -- the position is supposed to be set
       to the beginning of a SinglePos subtable [page 192] -- *)
  let offset_SinglePos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  d_fetch offset_SinglePos d_coverage d >>= fun coverage ->
  match posFormat with
  | 1 ->
      d_value_format d >>= fun valueFormat ->
      d_value_record valueFormat d >>= fun valueRecord ->
      return (SinglePosAdjustment1(coverage, valueRecord))

  | 2 ->
      d_value_format d >>= fun valueFormat ->
      d_list (d_value_record valueFormat) d >>= fun singleposlst ->
      combine_coverage d coverage singleposlst >>= fun comb ->
      return (SinglePosAdjustment2(comb))

  | _ -> err_version d (!% posFormat)


let d_pair_adjustment_subtable d : gpos_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of a PairPos subtable [page 194] -- *)
  let offset_PairPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  d_fetch offset_PairPos d_coverage d >>= fun coverage ->
  match posFormat with
  | 1 ->
      d_value_format d >>= fun valueFormat1 ->
      d_value_format d >>= fun valueFormat2 ->
      d_list (d_fetch offset_PairPos (d_pair_set valueFormat1 valueFormat2)) d >>= fun pairsetlst ->
      combine_coverage d coverage pairsetlst >>= fun comb ->
      return (PairPosAdjustment1(comb))

  | 2 ->
      d_value_format d >>= fun valueFormat1 ->
      d_value_format d >>= fun valueFormat2 ->
      d_fetch offset_PairPos d_class_definition d >>= fun classDef1 ->
      d_fetch offset_PairPos d_class_definition d >>= fun classDef2 ->
      d_uint16 d >>= fun class1Count ->
      d_uint16 d >>= fun class2Count ->
      d_repeat class1Count (d_class_1_record class2Count valueFormat1 valueFormat2) d >>= fun pairposlst ->
      begin
        try return (PairPosAdjustment2(classDef1, classDef2, numbering pairposlst)) with
        | Invalid_argument(_) -> err `Inconsistent_length_of_class
      end

  | _ -> err_version d (!% posFormat)


let d_anchor d : anchor ok =
  d_uint16 d >>= fun anchorFormat ->
  d_int16 d >>= fun xcoord ->
  d_int16 d >>= fun ycoord ->
  match anchorFormat with
  | 1 ->
      return (xcoord, ycoord, NoAnchorAdjustment)

  | 2 ->
      d_uint16 d >>= fun anchorPoint ->
      return (xcoord, ycoord, AnchorPointAdjustment(anchorPoint))

  | 3 ->
      d_device_table d >>= fun xdevtbl ->
      d_device_table d >>= fun ydevtbl ->
      return (xcoord, ycoord, DeviceAnchorAdjustment(xdevtbl, ydevtbl))

  | _ -> err_version d (!% anchorFormat)


let d_mark_record offset_MarkArray classCount d : mark_record ok =
  d_uint16 d >>= fun classId ->
  confirm (classId < classCount) (`Invalid_mark_class(classId)) >>= fun () ->
  d_fetch offset_MarkArray d_anchor d >>= fun anchor ->
  return (classId, anchor)


let d_mark_array classCount d : (mark_record list) ok =
  let offset_MarkArray = cur_pos d in
  d_list (d_mark_record offset_MarkArray classCount) d


let d_base_record offset_BaseArray classCount d : base_record ok =
  d_repeat classCount (d_fetch_opt offset_BaseArray d_anchor) d >>= fun anchorlst ->
  return (Array.of_list anchorlst)


let d_base_array classCount d : (base_record list) ok =
  let offset_BaseArray = cur_pos d in
  d_list (d_base_record offset_BaseArray classCount) d


let d_mark_to_base_attachment_subtable d =
  let offset_MarkBasePos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  match posFormat with
  | 1 ->
      d_fetch offset_MarkBasePos d_coverage d >>= fun markCoverage ->
      d_fetch offset_MarkBasePos d_coverage d >>= fun baseCoverage ->
      d_uint16 d >>= fun classCount ->
      d_fetch offset_MarkBasePos (d_mark_array classCount) d >>= fun markArray ->
      d_fetch offset_MarkBasePos (d_base_array classCount) d >>= fun baseArray ->
      combine_coverage d markCoverage markArray >>= fun mark_assoc ->
      combine_coverage d baseCoverage baseArray >>= fun base_assoc ->
      return (MarkBasePos1(classCount, mark_assoc, base_assoc))

  | _ -> err_version d (!% posFormat)


let d_component_record offset_LigatureAttach classCount d : component_record ok =
  d_repeat classCount (d_fetch_opt offset_LigatureAttach d_anchor) d >>= fun anchoroptlst ->
  return (Array.of_list anchoroptlst)


let d_ligature_attach classCount d : ligature_attach ok =
  let offset_LigatureAttach = cur_pos d in
  d_list (d_component_record offset_LigatureAttach classCount) d


let d_ligature_array classCount d : (ligature_attach list) ok =
  let offset_LigatureArray = cur_pos d in
  d_list (d_fetch offset_LigatureArray (d_ligature_attach classCount)) d


let d_mark_to_ligature_attachment_subtable d =
  let offset_MarkLigPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  match posFormat with
  | 1 ->
      d_fetch offset_MarkLigPos d_coverage d >>= fun markCoverage ->
      d_fetch offset_MarkLigPos d_coverage d >>= fun ligatureCoverage ->
      d_uint16 d >>= fun classCount ->
      d_fetch offset_MarkLigPos (d_mark_array classCount) d >>= fun markArray ->
      d_fetch offset_MarkLigPos (d_ligature_array classCount) d >>= fun ligatureArray ->
      combine_coverage d markCoverage markArray >>= fun mark_assoc ->
      combine_coverage d ligatureCoverage ligatureArray >>= fun ligature_assoc ->
      return (MarkLigPos1(classCount, mark_assoc, ligature_assoc))

  | _ -> err_version d (!% posFormat)


let d_mark2_record offset_Mark2Array classCount d : mark2_record ok =
  d_repeat classCount (d_fetch offset_Mark2Array d_anchor) d >>= fun anchorlst ->
  return (Array.of_list anchorlst)


let d_mark2_array  classCount d : (mark2_record list) ok =
  let offset_Mark2Array = cur_pos d in
  d_list (d_mark2_record offset_Mark2Array classCount) d


let d_mark_to_mark_attachment_subtable d =
  let offset_MarkToMarkPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  match posFormat with
  | 1 ->
      d_fetch offset_MarkToMarkPos d_coverage d >>= fun mark1Coverage ->
      d_fetch offset_MarkToMarkPos d_coverage d >>= fun mark2Coverage ->
      d_uint16 d >>= fun classCount ->
      d_fetch offset_MarkToMarkPos (d_mark_array classCount) d >>= fun markArray ->
      d_fetch offset_MarkToMarkPos (d_mark2_array classCount) d >>= fun mark2Array ->
      combine_coverage d mark1Coverage markArray >>= fun mark_assoc ->
      combine_coverage d mark2Coverage mark2Array >>= fun mark2_assoc ->
      return (MarkMarkPos1(classCount, mark_assoc, mark2_assoc))

  | _ -> err_version d (!% posFormat)


let lookup_gpos_exact offsetlst_SubTable lookupType d : (gpos_subtable list) ok =
  match lookupType with
  | 1 ->  (* -- Single adjustment -- *)
      seek_every_pos offsetlst_SubTable d_single_adjustment_subtable d

  | 2 ->  (* -- Pair adjustment -- *)
      Format.fprintf fmtGSUB "number of subtables = %d@," (List.length offsetlst_SubTable);  (* for debug *)
      seek_every_pos offsetlst_SubTable d_pair_adjustment_subtable d

  | 3 ->  (* -- Cursive attachment -- *)
      return []  (* temporarily unsupported *)

  | 4 ->  (* -- MarkToBase attachment -- *)
      seek_every_pos offsetlst_SubTable d_mark_to_base_attachment_subtable d

  | 5 ->  (* -- MarkToLigature attachment -- *)
      seek_every_pos offsetlst_SubTable d_mark_to_ligature_attachment_subtable d

  | 6 ->  (* -- MarkToMark attachment -- *)
      seek_every_pos offsetlst_SubTable d_mark_to_mark_attachment_subtable d

  | 7 ->
      return []  (* temporarily unsupported *)

  | 8 ->
      return []  (* temporarily unsupported *)

  | 9 ->  (* -- Extension positioning [page 213] -- *)
      err `Invalid_extension_position

  | _ ->
      err (`Invalid_GPOS_lookup_type(lookupType))


let d_extension_position d : (gpos_subtable list) ok =
    (* -- the position is supposed to be set
          to the beginning of ExtensionPosFormat1 subtable [page 213] -- *)
  let offset_ExtensionPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  confirm (posFormat = 1) (e_version d (!% posFormat)) >>= fun () ->
  d_uint16 d >>= fun extensionLookupType ->
  d_uint32_int d >>= fun extensionOffset ->
  let offset = offset_ExtensionPos + extensionOffset in
  lookup_gpos_exact [offset] extensionLookupType d


let lookup_gpos d : (gpos_subtable list) ok =
    (* -- the position is supposed to be set
          to the beginning of Lookup table [page 137] -- *)
  let offset_Lookup_table = cur_pos d in
  d_uint16 d >>= fun lookupType ->
  Format.fprintf fmtGSUB "# lookupType = %d@," lookupType;  (* for debug *)
  d_uint16 d >>= fun lookupFlag ->
  Format.fprintf fmtGSUB "# lookupFlag = %d@," lookupFlag;  (* for debug *)
(*
  let rightToLeft      = 0 < lookupFlag land 1 in
  let ignoreBaseGlyphs = 0 < lookupFlag land 2 in
  let ignoreLigatures  = 0 < lookupFlag land 4 in
  let ignoreMarks      = 0 < lookupFlag land 8 in
*)
  d_offset_list offset_Lookup_table d >>= fun offsetlst_SubTable ->
  match lookupType with
  | 9 ->
      seek_every_pos offsetlst_SubTable d_extension_position d >>= fun subtablelstlst ->
      return (List.concat subtablelstlst)

  | _ ->
      lookup_gpos_exact offsetlst_SubTable lookupType d


type 'a folding_gpos_single1 = 'a -> glyph_id list -> value_record -> 'a

type 'a folding_gpos_single2 = 'a -> glyph_id * value_record -> 'a

type 'a folding_gpos_pair1 = 'a -> glyph_id * (glyph_id * value_record * value_record) list -> 'a

type 'a folding_gpos_pair2 = class_definition list -> class_definition list -> 'a -> (class_value * (class_value * value_record * value_record) list) list -> 'a

type 'a folding_gpos_markbase1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * base_record) list -> 'a

type 'a folding_gpos_marklig1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * ligature_attach) list -> 'a

type 'a folding_gpos_markmark1 = int -> 'a -> (glyph_id * mark_record) list -> (glyph_id * mark2_record) list -> 'a

let rec fold_subtables_gpos
    (f_single1 : 'a folding_gpos_single1)
    (f_single2 : 'a folding_gpos_single2)
    (f_pair1 : 'a folding_gpos_pair1)
    (f_pair2 : 'a folding_gpos_pair2)
    (f_markbase1 : 'a folding_gpos_markbase1)
    (f_marklig1 : 'a folding_gpos_marklig1)
    (f_markmark1 : 'a folding_gpos_markmark1)
    (init : 'a) (subtablelst : gpos_subtable list) : 'a =
  let iter = fold_subtables_gpos f_single1 f_single2 f_pair1 f_pair2 f_markbase1 f_marklig1 f_markmark1 in
    match subtablelst with
    | [] ->
        init

    | SinglePosAdjustment1(coverage, valreclst) :: tail ->
        let initnew = f_single1 init coverage valreclst in
          iter initnew tail

    | SinglePosAdjustment2(assoc) :: tail ->
        let initnew = List.fold_left f_single2 init assoc in
          iter initnew tail

    | PairPosAdjustment1(gidfst_pairposlst_assoc) :: tail ->
        let initnew = List.fold_left f_pair1 init gidfst_pairposlst_assoc in
          iter initnew tail

    | PairPosAdjustment2(clsdeflst1, clsdeflst2, cls_pairposlst_assoc) :: tail ->
        let initnew = f_pair2 clsdeflst1 clsdeflst2 init cls_pairposlst_assoc in
          iter initnew tail

    | MarkBasePos1(classCount, mark_assoc, base_assoc) :: tail ->
        let initnew = f_markbase1 classCount init mark_assoc base_assoc in
          iter initnew tail

    | MarkLigPos1(classCount, mark_assoc, ligature_assoc) :: tail ->
        let initnew = f_marklig1 classCount init mark_assoc ligature_assoc in
          iter initnew tail

    | MarkMarkPos1(classCount, mark_assoc, mark2_assoc) :: tail ->
        let initnew = f_markmark1 classCount init mark_assoc mark2_assoc in
          iter initnew tail


let gpos feature
    ?single1:(f_single1 = (fun x _ _ -> x))
    ?single2:(f_single2 = (fun x _ -> x))
    ?pair1:(f_pair1 = (fun x _ -> x))
    ?pair2:(f_pair2 = (fun _ _ x _ -> x))
    ?markbase1:(f_markbase1 = (fun _ x _ _ -> x))
    ?marklig1:(f_marklig1 = (fun _ x _ _ -> x))
    ?markmark1:(f_markmark1 = (fun _ x _ _ -> x))
    init =
  gxxx_subtable_list lookup_gpos feature >>= fun subtablelstlst ->
  let subtablelst = List.concat subtablelstlst in
  return (fold_subtables_gpos f_single1 f_single2 f_pair1 f_pair2 f_markbase1 f_marklig1 f_markmark1 init subtablelst)


(* -- MATH table -- *)

type math_value_record = int * device_table option

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

type math_kern = math_value_record list * math_value_record list

type math_kern_info_record =
  {
    top_right_math_kern    : math_kern option;
    top_left_math_kern     : math_kern option;
    bottom_right_math_kern : math_kern option;
    bottom_left_math_kern  : math_kern option;
  }

type math_glyph_info =
  {
    math_italics_correction    : (glyph_id * math_value_record) list;
    math_top_accent_attachment : (glyph_id * math_value_record) list;
    math_kern_info             : (glyph_id * math_kern_info_record) list;
  }

type glyph_part_record =
  {
    glyph_id_for_part      : glyph_id;
    start_connector_length : int;
    end_connector_length   : int;
    full_advance           : int;
    part_flags             : int;
  }

type math_glyph_construction =
  {
    glyph_assembly                 : (math_value_record * glyph_part_record list) option;
    math_glyph_variant_record_list : (glyph_id * int) list;
  }

type math_variants =
  {
    min_connector_overlap : int;
    vert_glyph_assoc      : (glyph_id * math_glyph_construction) list;
    horiz_glyph_assoc     : (glyph_id * math_glyph_construction) list;
  }

type math =
  {
    math_constants  : math_constants;
    math_glyph_info : math_glyph_info;
    math_variants   : math_variants;
  }


let d_math_value_record offset_origin d : math_value_record ok =
  d_int16 d >>= fun value ->
  d_fetch_opt offset_origin d_device_table d >>= fun device_table_opt ->
  return (value, device_table_opt)


let d_math_constants d : math_constants ok =
  let offset_origin = cur_pos d in
  let dm = d_math_value_record offset_origin in
  d_int16  d >>= fun script_percent_scale_down ->
  d_int16  d >>= fun script_script_percent_scale_down ->
  d_uint16 d >>= fun delimited_sub_formula_min_height ->
  d_uint16 d >>= fun display_operator_min_height ->
  dm       d >>= fun math_leading ->
  dm       d >>= fun axis_height ->
  dm       d >>= fun accent_base_height ->
  dm       d >>= fun flattened_accent_base_height ->
  dm       d >>= fun subscript_shift_down ->
  dm       d >>= fun subscript_top_max ->
  dm       d >>= fun subscript_baseline_drop_min ->
  dm       d >>= fun superscript_shift_up ->
  dm       d >>= fun superscript_shift_up_cramped ->
  dm       d >>= fun superscript_bottom_min ->
  dm       d >>= fun superscript_baseline_drop_max ->
  dm       d >>= fun sub_superscript_gap_min ->
  dm       d >>= fun superscript_bottom_max_with_subscript ->
  dm       d >>= fun space_after_script ->
  dm       d >>= fun upper_limit_gap_min ->
  dm       d >>= fun upper_limit_baseline_rise_min ->
  dm       d >>= fun lower_limit_gap_min ->
  dm       d >>= fun lower_limit_baseline_drop_min ->
  dm       d >>= fun stack_top_shift_up ->
  dm       d >>= fun stack_top_display_style_shift_up ->
  dm       d >>= fun stack_bottom_shift_down ->
  dm       d >>= fun stack_bottom_display_style_shift_down ->
  dm       d >>= fun stack_gap_min ->
  dm       d >>= fun stack_display_style_gap_min ->
  dm       d >>= fun stretch_stack_top_shift_up ->
  dm       d >>= fun stretch_stack_bottom_shift_down ->
  dm       d >>= fun stretch_stack_gap_above_min ->
  dm       d >>= fun stretch_stack_gap_below_min ->
  dm       d >>= fun fraction_numerator_shift_up ->
  dm       d >>= fun fraction_numerator_display_style_shift_up ->
  dm       d >>= fun fraction_denominator_shift_down ->
  dm       d >>= fun fraction_denominator_display_style_shift_down ->
  dm       d >>= fun fraction_numerator_gap_min ->
  dm       d >>= fun fraction_num_display_style_gap_min ->
  dm       d >>= fun fraction_rule_thickness ->
  dm       d >>= fun fraction_denominator_gap_min ->
  dm       d >>= fun fraction_denom_display_style_gap_min ->
  dm       d >>= fun skewed_fraction_horizontal_gap ->
  dm       d >>= fun skewed_fraction_vertical_gap ->
  dm       d >>= fun overbar_vertical_gap ->
  dm       d >>= fun overbar_rule_thickness ->
  dm       d >>= fun overbar_extra_ascender ->
  dm       d >>= fun underbar_vertical_gap ->
  dm       d >>= fun underbar_rule_thickness ->
  dm       d >>= fun underbar_extra_descender ->
  dm       d >>= fun radical_vertical_gap ->
  dm       d >>= fun radical_display_style_vertical_gap ->
  dm       d >>= fun radical_rule_thickness ->
  dm       d >>= fun radical_extra_ascender ->
  dm       d >>= fun radical_kern_before_degree ->
  dm       d >>= fun radical_kern_after_degree ->
  d_int16  d >>= fun radical_degree_bottom_raise_percent ->
  return {
    script_percent_scale_down                     ;
    script_script_percent_scale_down              ;
    delimited_sub_formula_min_height              ;
    display_operator_min_height                   ;
    math_leading                                  ;
    axis_height                                   ;
    accent_base_height                            ;
    flattened_accent_base_height                  ;
    subscript_shift_down                          ;
    subscript_top_max                             ;
    subscript_baseline_drop_min                   ;
    superscript_shift_up                          ;
    superscript_shift_up_cramped                  ;
    superscript_bottom_min                        ;
    superscript_baseline_drop_max                 ;
    sub_superscript_gap_min                       ;
    superscript_bottom_max_with_subscript         ;
    space_after_script                            ;
    upper_limit_gap_min                           ;
    upper_limit_baseline_rise_min                 ;
    lower_limit_gap_min                           ;
    lower_limit_baseline_drop_min                 ;
    stack_top_shift_up                            ;
    stack_top_display_style_shift_up              ;
    stack_bottom_shift_down                       ;
    stack_bottom_display_style_shift_down         ;
    stack_gap_min                                 ;
    stack_display_style_gap_min                   ;
    stretch_stack_top_shift_up                    ;
    stretch_stack_bottom_shift_down               ;
    stretch_stack_gap_above_min                   ;
    stretch_stack_gap_below_min                   ;
    fraction_numerator_shift_up                   ;
    fraction_numerator_display_style_shift_up     ;
    fraction_denominator_shift_down               ;
    fraction_denominator_display_style_shift_down ;
    fraction_numerator_gap_min                    ;
    fraction_num_display_style_gap_min            ;
    fraction_rule_thickness                       ;
    fraction_denominator_gap_min                  ;
    fraction_denom_display_style_gap_min          ;
    skewed_fraction_horizontal_gap                ;
    skewed_fraction_vertical_gap                  ;
    overbar_vertical_gap                          ;
    overbar_rule_thickness                        ;
    overbar_extra_ascender                        ;
    underbar_vertical_gap                         ;
    underbar_rule_thickness                       ;
    underbar_extra_descender                      ;
    radical_vertical_gap                          ;
    radical_display_style_vertical_gap            ;
    radical_rule_thickness                        ;
    radical_extra_ascender                        ;
    radical_kern_before_degree                    ;
    radical_kern_after_degree                     ;
    radical_degree_bottom_raise_percent           ;
  }


let d_math_italics_correction_info d : ((glyph_id * math_value_record) list) ok =
  let offset_MathItalicCollectionInfo_table = cur_pos d in
  d_fetch offset_MathItalicCollectionInfo_table d_coverage d >>= fun coverage ->
  d_list (d_math_value_record offset_MathItalicCollectionInfo_table) d >>= fun mvrlst ->
  combine_coverage d coverage mvrlst


let d_math_top_accent_attachment d : ((glyph_id * math_value_record) list) ok =
  let offset_MathTopAccentAttachment_table = cur_pos d in
  d_fetch offset_MathTopAccentAttachment_table d_coverage d >>= fun coverage ->
  d_list (d_math_value_record offset_MathTopAccentAttachment_table) d >>= fun mvrlst ->
  combine_coverage d coverage mvrlst


let d_math_kern d : math_kern ok =
  let offset_MathKern_table = cur_pos d in
  d_uint16 d >>= fun heightCount ->
  d_repeat heightCount (d_math_value_record offset_MathKern_table) d >>= fun correctionHeight_lst ->
  d_repeat (heightCount + 1) (d_math_value_record offset_MathKern_table) d >>= fun kernValue_lst ->
  return (correctionHeight_lst, kernValue_lst)


let d_math_kern_info_record offset_MathKernInfo_table d : math_kern_info_record ok =
  Format.fprintf fmtMATH "### MathKernInfoRecord[1] = %d@," (cur_pos d);
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun topRightMathKern_opt ->
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun topLeftMathKern_opt ->
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun bottomRightMathKern_opt ->
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun bottomLeftMathKern_opt ->
  return {
    top_right_math_kern    = topRightMathKern_opt;
    top_left_math_kern     = topLeftMathKern_opt;
    bottom_right_math_kern = bottomRightMathKern_opt;
    bottom_left_math_kern  = bottomLeftMathKern_opt;
  }


let d_math_kern_info d : ((glyph_id * math_kern_info_record) list) ok =
  let offset_MathKernInfo_table = cur_pos d in
  Format.fprintf fmtMATH "## MathKernInfo[1] = %d@," (cur_pos d);
  d_fetch offset_MathKernInfo_table d_coverage d >>= fun coverage ->
  Format.fprintf fmtMATH "## MathKernInfo[2] = %d@," (cur_pos d);
  d_list (d_math_kern_info_record offset_MathKernInfo_table) d >>= fun mvrlst ->
  Format.fprintf fmtMATH "## MathKernInfo[3] = %d@," (cur_pos d);
  combine_coverage d coverage mvrlst


let d_math_glyph_info d : math_glyph_info ok =
  let offset_MathGlyphInfo_table = cur_pos d in
  Format.fprintf fmtMATH "# jump to MathItalicsCorrection = %d@," (cur_pos d);
  d_fetch offset_MathGlyphInfo_table d_math_italics_correction_info d >>= fun mathItalicsCorrection ->
  Format.fprintf fmtMATH "# jump to MathTopAccentAttachment = %d@," (cur_pos d);
  d_fetch offset_MathGlyphInfo_table d_math_top_accent_attachment d >>= fun mathTopAccentAttachment ->
  d_fetch_opt offset_MathGlyphInfo_table d_coverage d >>= fun _ ->
  Format.fprintf fmtMATH "# jump to MathKernInfo = %d@," (cur_pos d);
  d_fetch_list offset_MathGlyphInfo_table d_math_kern_info d >>= fun mathKernInfo ->
  Format.fprintf fmtMATH "# END MathGlyphInfo@,";
  return {
    math_italics_correction    = mathItalicsCorrection;
    math_top_accent_attachment = mathTopAccentAttachment;
    math_kern_info             = mathKernInfo;
  }


let d_math_glyph_variant_record d : (glyph_id * int) ok =
  d_uint16 d >>= fun variantGlyph ->
  d_uint16 d >>= fun advanceMeasurement ->
  return (variantGlyph, advanceMeasurement)


let d_glyph_part_record d : glyph_part_record ok =
  d_uint16 d >>= fun glyph ->
  d_uint16 d >>= fun startConnectorLength ->
  d_uint16 d >>= fun endConnectorLength ->
  d_uint16 d >>= fun fullAdvance ->
  d_uint16 d >>= fun partFlags ->
  return {
    glyph_id_for_part      = glyph;
    start_connector_length = startConnectorLength;
    end_connector_length   = endConnectorLength;
    full_advance           = fullAdvance;
    part_flags             = partFlags;
  }


let d_glyph_assembly d : (math_value_record * glyph_part_record list) ok =
  let offset_GlyphAssembly_table = cur_pos d in
  d_math_value_record offset_GlyphAssembly_table d >>= fun italicsCorrection ->
  d_list d_glyph_part_record d >>= fun partRecords_lst ->
  return (italicsCorrection, partRecords_lst)


let d_math_glyph_construction d : math_glyph_construction ok =
  let offset_MathGlyphConstruction_table = cur_pos d in
  Format.fprintf fmtMATH "| | {GlyphAssembly@,";
  d_fetch_opt offset_MathGlyphConstruction_table d_glyph_assembly d >>= fun glyphAssembly ->
  Format.fprintf fmtMATH "| | MathGlyphVariantRecord@,";
  d_list d_math_glyph_variant_record d >>= fun mathGlyphVariantRecord_lst ->
  Format.fprintf fmtMATH "| | END MathGlyphConstruction}@,";
  return {
    glyph_assembly                 = glyphAssembly;
    math_glyph_variant_record_list = mathGlyphVariantRecord_lst;
  }


let d_math_variants d : math_variants ok =
  let offset_MathVariants_table = cur_pos d in
  d_uint16 d >>= fun minConnectorOverlap ->
  Format.fprintf fmtMATH "| VertGlyphCoverage@,";
  d_fetch offset_MathVariants_table d_coverage d >>= fun vertGlyphCoverage ->
  Format.fprintf fmtMATH "| HorizGlyphCoverage@,";
  d_fetch offset_MathVariants_table d_coverage d >>= fun horizGlyphCoverage ->
  d_uint16 d >>= fun vertGlyphCount ->
  d_uint16 d >>= fun horizGlyphCount ->
  let df = d_fetch offset_MathVariants_table d_math_glyph_construction in
  Format.fprintf fmtMATH "| VertGlyphConstruction@,";
  d_repeat vertGlyphCount df d >>= fun vertGlyphConstruction_lst ->
  Format.fprintf fmtMATH "| HorizGlyphConstruction@,";
  d_repeat horizGlyphCount df d >>= fun horizGlyphConstruction_lst ->
  combine_coverage d vertGlyphCoverage vertGlyphConstruction_lst >>= fun vertcomb ->
  combine_coverage d horizGlyphCoverage horizGlyphConstruction_lst >>= fun horizcomb ->
  return {
    min_connector_overlap = minConnectorOverlap;
    vert_glyph_assoc      = vertcomb;
    horiz_glyph_assoc     = horizcomb;
  }


let math (decoder : decoder) : (math option) ok =
  let cd = common decoder in
  init_decoder cd >>= fun () ->
  seek_table Tag.math cd >>= function
    | None ->
        return None

    | Some(_) ->
        let offset_MATH = cur_pos cd in
        Format.fprintf fmtMATH "begin MATH = %d@," offset_MATH;
        d_uint32 cd >>= fun version ->
        confirm (version = !%% 0x00010000L) (e_version cd version) >>= fun () ->
        Format.fprintf fmtMATH "jump to MathConstants = %d@," (cur_pos cd);
        d_fetch offset_MATH d_math_constants cd >>= fun mathConstants ->
        Format.fprintf fmtMATH "jump to MathGlyphInfo = %d@," (cur_pos cd);
        d_fetch offset_MATH d_math_glyph_info cd >>= fun mathGlyphInfo ->
        Format.fprintf fmtMATH "jump to MathVariants = %d@," (cur_pos cd);
        d_fetch offset_MATH d_math_variants cd >>= fun mathVariants ->
        Format.fprintf fmtMATH "end MATH@,";
        let math =
          {
            math_constants  = mathConstants;
            math_glyph_info = mathGlyphInfo;
            math_variants   = mathVariants;
          }
        in
        return (Some(math))


(* -- BASE table -- *)

let base d =
  let offset_BASE = cur_pos d in
  d_uint32 d >>= fun version ->
  confirm (version = !%% 0x00010000L) (e_version d version) >>= fun () ->
  d_offset_opt offset_BASE d >>= fun offsetopt_HorizAxis ->
  d_offset_opt offset_BASE d >>= fun offsetopt_VertAxis ->
  return ()  (* temporary *)


(* -- CFF_ table -- *)



let set_uint16 bytes offset ui =
  let b0 = ui / 256 in
  let b1 = ui mod 256 in
  begin
    Bytes.set bytes offset       (Char.chr b0);
    Bytes.set bytes (offset + 1) (Char.chr b1);
  end


module Encode = struct

  type raw_table = {
    table_tag            : tag;
    table_content_length : int;
    table_padded_length  : int;
    table_checksum       : wint;
    table_data           : string;
  }

  type encoder = {
    buffer : Buffer.t;
  }

  let create_encoder () =
    { buffer = Buffer.create 0x10000; }
      (* -- the initial size is an arbitrary positive number -- *)

(*
  let make_raw_glyph aw lsb bbox data =
    let len = String.length data in
      {
        glyph_aw          = aw;
        glyph_lsb         = lsb;
        glyph_bbox        = bbox;
        glyph_data        = data;
        glyph_data_length = len;
      }
*)
  let pad_data s len =
    let r = (4 - len mod 4) mod 4 in
    let sp = s ^ (String.make r (Char.chr 0)) in
    (sp, len + r)


  let add_checksum (x : wint) (y : wint) : wint =
    let open WideInt in
      let q = (of_int 1) lsl 32 in
      (x +% y) mod q


  let table_checksum (sp : string) (lenp : int) : wint =
    let open WideInt in
      let rec aux acc i =
        if i >= lenp then acc else
          let b0 = of_byte (String.get sp i) in
          let b1 = of_byte (String.get sp (i + 1)) in
          let b2 = of_byte (String.get sp (i + 2)) in
          let b3 = of_byte (String.get sp (i + 3)) in
          let ui = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
          let accnew = add_checksum acc ui in
          aux accnew (i + 4)
      in
      aux (of_int 0) 0


  let to_raw_table tag enc =
    let buf = enc.buffer in
    let len = Buffer.length buf in
    let s = Buffer.contents buf in
    let (sp, lenp) = pad_data s len in
    let chksum = table_checksum sp lenp in
(*
    Printf.printf "table checksum: %d@," chksum;
*)
      {
        table_tag            = tag;
        table_content_length = len;
        table_padded_length  = lenp;
        table_checksum       = chksum;
        table_data           = sp;
      }


  let compare_table rawtbl1 rawtbl2 =
    Tag.compare rawtbl1.table_tag rawtbl2.table_tag


  let enc_byte enc ch =
      Buffer.add_char enc.buffer ch


  let enc_pad enc n =
    let rec aux = function
      | 0 -> return ()
      | x -> (Buffer.add_char enc.buffer (Char.chr 0); aux (x - 1))
    in
    aux n


  let enc_uint16_unsafe enc ui =
    let b0 = ui lsr 8 in
    let b1 = ui - (b0 lsl 8) in
(*
    Printf.printf "uint16 %d --> (%d, %d)@," ui b0 b1;
*)
    begin
      enc_byte enc (Char.chr b0);
      enc_byte enc (Char.chr b1);
    end


  let enc_uint24_unsafe enc ui =
    let b0 = ui lsr 16 in
    let r0 = ui - (b0 lsl 16) in
    let b1 = r0 lsr 8 in
    let b2 = r0 - (b1 lsl 8) in
    begin
      enc_byte enc (Char.chr b0);
      enc_byte enc (Char.chr b1);
      enc_byte enc (Char.chr b2);
    end


  let enc_uint32_unsafe enc (ui : wint) =
    let (b0, b1, b2, b3) = cut_uint32_unsafe ui in
    begin
      enc_byte enc b0;
      enc_byte enc b1;
      enc_byte enc b2;
      enc_byte enc b3;
    end


  let enc_uint8 enc (ui : int) =
    if not (0 <= ui && ui < 256) then
      err (`Not_encodable_as_uint8(ui))
    else
      begin
        enc_byte enc (Char.chr ui);
        return ()
      end


  let enc_uint16 enc ui =
    if not (0 <= ui && ui < 0x10000) then
      err (`Not_encodable_as_uint16(ui))
    else
      begin
        enc_uint16_unsafe enc ui;
        return ()
      end


  let enc_int16 enc i =
    if not (-0x8000 <= i && i < 0x8000) then
      err (`Not_encodable_as_int16(i))
    else
      let ui = if i < 0 then i + 0x10000 else i in
      begin
        enc_uint16_unsafe enc ui;
        return ()
      end


  let enc_uint24 enc ui =
    if not (0 <= ui && ui < 0x1000000) then
      err (`Not_encodable_as_uint24(ui))
    else
      begin
        enc_uint24_unsafe enc ui;
        return ()
      end


  let enc_uint32 enc ui =
    let open WideInt in
      if not (is_in_uint32 ui) then
        err (`Not_encodable_as_uint32(ui))
      else
        begin
          enc_uint32_unsafe enc ui;
          return ()
        end


  let enc_int32 enc i =
    let open WideInt in
    if not (is_in_int32 i) then
      err (`Not_encodable_as_int32(i))
    else
      let ui = if is_neg i then i +% (!%% 0x100000000L) else i in
(*
      Printf.printf "i32 -> u32 (%d ---> %d)@," i ui;
*)
      begin
        enc_uint32_unsafe enc ui;
        return ()
      end


  let enc_time enc itime =
    let open WideInt in
      if not (is_in_int64 itime) then
        (* -- does NOT allow negative value for clarity -- *)
        err (`Not_encodable_as_time(itime))
      else
        let wi = if is_neg itime then !% 0 else itime in  (* temporary *)
        let q0 = wi lsr 32 in
        let q1 = wi -% (q0 lsl 32) in
(*
      Printf.printf "time %s ---> (%d, %d)@," (Int64.to_string ui64) q0 q1;
*)
      begin
        enc_uint32_unsafe enc q0;
        enc_uint32_unsafe enc q1;
        return ()
      end


  let enc_direct enc s =
    begin
(*
      Printf.printf "direct \"%s\"@," s;
*)
      Buffer.add_string enc.buffer s;
      return ()
    end


  let sum_of_list  f lst = lst |> List.fold_left (fun acc elem -> acc + f elem) 0

  let sum_of_array f arr = arr |> Array.fold_left (fun acc elem -> acc + f elem) 0


  let int_of_offsize ofsz =
    match ofsz with
    | OffSize1 -> 1
    | OffSize2 -> 2
    | OffSize3 -> 3
    | OffSize4 -> 4


  let calculate_offsize maxoff =
    if      maxoff < 256      then OffSize1
    else if maxoff < 65536    then OffSize2
    else if maxoff < 16777216 then OffSize3
    else                           OffSize4


  let enc_offsize enc ofsz =
    enc_uint8 enc (int_of_offsize ofsz)


  let enc_cff_offset ofsz enc (off : wint) =
    match ofsz with
    | OffSize1 -> enc_uint8  enc (WideInt.to_int off)
    | OffSize2 -> enc_uint16 enc (WideInt.to_int off)
    | OffSize3 -> enc_uint24 enc (WideInt.to_int off)
    | OffSize4 -> enc_uint32 enc off


  let enc_index_singleton enc ef len data =
    enc_uint16 enc 1 >>= fun () ->
    let ofsz = calculate_offsize (len + 1) in
    enc_offsize    enc ofsz             >>= fun () ->
    enc_cff_offset ofsz enc !%1         >>= fun () ->
    enc_cff_offset ofsz enc !%(len + 1) >>= fun () ->
    ef                  enc data


  (* -- `enc_index`: encodes `elems`, a list of pairs of an element and its byte length, into an INDEX -- *)
  let enc_index (type a) (enc : encoder) (ef : encoder -> a -> unit ok) (elems : (a * int) array) =
    let count   = Array.length elems in
    let datalen = sum_of_array snd elems in
    let ofsz    = calculate_offsize (datalen + 1) in
    enc_uint16 enc count >>= fun () ->
    if count = 0 then
      return ()
    else
      enc_offsize enc ofsz >>= fun () ->

      elems |> Array.fold_left (fun res (_, len) ->
        res >>= fun offset ->
        enc_cff_offset ofsz enc !%offset >>= fun () ->
        return (offset + len)
      ) (return 1) >>= fun last_offset ->

      enc_cff_offset ofsz enc !%last_offset >>= fun () ->

      elems |> Array.fold_left (fun res (data, _) ->
        res >>= fun () ->
        ef enc data
      ) (return ())


  let enc_copy_direct d enc offset len =
    seek_pos offset d >>= fun () ->
    d_bytes len d >>= fun data ->
    enc_direct enc data


  let enc_array enc ef arr =
    arr |> Array.fold_left (fun res x -> res >>= fun () -> ef enc x) (return ())


  let enc_opt enc encf = function
    | None    -> return ()
    | Some(v) -> encf enc v


  let enc_charset_identity enc nGlyphs =
    enc_uint8  enc 2 (* Format 2 *) >>= fun () ->
    enc_uint16 enc 1                >>= fun () ->
    enc_uint16 enc (nGlyphs - 2)


  let enc_fdselect_format0 enc fdselect =
    enc_uint8 enc 0 >>= fun () ->
    fdselect |> Array.fold_left (fun res sel ->
      res >>= fun () ->
      enc_uint8 enc sel
    ) (return ())


  let nibbles_of_float fl =
    let str = Str.global_replace (Str.regexp "E-") "M" (Format.sprintf "%G" fl) in
    let clst = List.init (String.length str) (String.get str) in
    clst |> List.map (function
      | '.'                           -> 0xa
      | 'E'                           -> 0xb
      | 'M' (* this means "E-" *)     -> 0xc
      | '-'                           -> 0xe
      | c when ('0' <= c && c <= '9') -> (Char.code c) - (Char.code '0')
      | _                             -> failwith "bug: nibbles_of_float" )


  let enc_nibbles enc nlst =
    let rec aux lst =
      match lst with
      | n0 :: n1 :: rest ->
          enc_uint8 enc ((n0 lsl 4) lor n1) >>= fun () ->
          aux rest

      | n0 :: [] ->
          enc_uint8 enc ((n0 lsl 4) lor 0xf)

      | [] ->
          enc_uint8 enc 0xff

    in
    aux nlst


  let enc_dict_element ~is_fixed_int (* if true, always encode integers into 5 byte *) enc elem =
    match elem with
    | Value(Integer(i)) when is_fixed_int ->
        enc_uint8 enc 29 >>= fun () ->
        enc_int32 enc !%i

    | Value(Integer(i)) when i |> is_in_range (-107) 107 ->
        enc_uint8 enc (i + 139)

    | Value(Integer(i)) when i |> is_in_range 108 1131 ->
        enc_uint8 enc (247 + ((i - 108) / 256)) >>= fun () ->
        enc_uint8 enc ((i - 108) mod 256)

    | Value(Integer(i)) when i |> is_in_range (-1131) (-108) ->
        enc_uint8 enc (254 - ((i + 1131) / 256)) >>= fun () ->
        enc_uint8 enc (255 - ((i + 1131) mod 256))

    | Value(Integer(i)) when i |> is_in_range (-32768) 32767 ->
        enc_uint8 enc 28 >>= fun () ->
        enc_int16 enc i

    | Value(Integer(i)) ->
        enc_uint8 enc 29 >>= fun () ->
        enc_int32 enc !%i

    | Value(Real(f)) ->
        enc_uint8 enc 30 >>= fun () ->
        enc_nibbles enc (nibbles_of_float f)

    | Key(ShortKey(i)) when i |> is_in_range 0 11 ->
        enc_uint8 enc i

    | Key(ShortKey(i)) when i |> is_in_range 13 21 ->
        enc_uint8 enc i

    | Key(LongKey(i)) ->
        enc_uint8 enc 12 >>= fun () ->
        enc_uint8 enc i

    | _ ->
        err `Invalid_cff_not_an_element


  let enc_dict ~is_fixed_int enc dictmap =
    let enc_dict_entry k vlst =
      vlst |> List.fold_left (fun res v ->
        res >>= fun () ->
        enc_dict_element ~is_fixed_int enc (Value(v))
      ) (return ()) >>= fun () ->
      enc_dict_element ~is_fixed_int enc (Key(k))
    in

    (* -- ROS or SyntheticBase operator should be the beginning of the DICT. [CFF p.17] -- *)
    let ros_key = LongKey(30) in
    let syntheticbase_key = LongKey(20) in
    let beginning =
      if DictMap.mem ros_key dictmap then
        Some(ros_key)
      else if DictMap.mem syntheticbase_key dictmap then
        Some(syntheticbase_key)
      else
        None
    in
    begin
      match beginning with
      | None ->
          return dictmap

      | Some(key) ->
          enc_dict_entry key (DictMap.find key dictmap) >>= fun () ->
          return (DictMap.remove key dictmap)

    end >>= fun dictmap ->

    DictMap.fold (fun k vlst res ->
      res >>= fun () ->
      enc_dict_entry k vlst
    ) dictmap (return ())


  let calculate_encoded_dict_element_length ~is_fixed_int elem =
    match elem with
    | Value(Integer(i)) when is_fixed_int                    -> 5
    | Value(Integer(i)) when i |> is_in_range (-107) 107     -> 1
    | Value(Integer(i)) when i |> is_in_range 108 1131       -> 2
    | Value(Integer(i)) when i |> is_in_range (-1131) (-108) -> 2
    | Value(Integer(i)) when i |> is_in_range (-32768) 32767 -> 3
    | Value(Integer(i))                                      -> 5
    | Value(Real(f)) ->
        let nlen = List.length (nibbles_of_float f) in
        let nlen = if nlen mod 2 = 0 then nlen + 2 else nlen + 1 in
        (1 + (nlen / 2))
    | Key(ShortKey(i)) when i |> is_in_range 0 11            -> 1
    | Key(ShortKey(i)) when i |> is_in_range 13 21           -> 1
    | Key(LongKey(i))                                        -> 2
    | _       -> failwith "calcutate_encoded_dict_element_length"


  let calculate_encoded_dict_length ~is_fixed_int dictmap =
    DictMap.fold (fun k vlst sum ->
      let sum =
        vlst |> List.fold_left (fun acc v ->
          acc + calculate_encoded_dict_element_length ~is_fixed_int (Value(v))
        ) sum
      in
      sum + calculate_encoded_dict_element_length ~is_fixed_int (Key(k))
    ) dictmap 0


  let calculate_index_length count datalen =
    if count = 0 then
      2
    else
      let ofsz = int_of_offsize (calculate_offsize (datalen + 1)) in
      2 + 1 + ofsz * (count + 1) + datalen


  let calculate_index_length_of_array f arr =
    calculate_index_length (Array.length arr) (sum_of_array f arr)


  let calculate_header_constants numTables =
    let rec aux n a =
      let anext = a * 2 in
      if anext <= numTables then
        aux (n + 1) anext
      else
        (a * 16, n)
    in
    aux 0 1


  type outline_type =
    | TrueTypeOutline
    | CFFData


  let make_font_file oltype rawtbllst =
    let sfntVersion =
      match oltype with
      | TrueTypeOutline -> !% 0x00010000
      | CFFData         -> !% 0x4F54544F
    in
    let numTables = List.length rawtbllst in
    let (searchRange, entrySelector) = calculate_header_constants numTables in
    let rangeShift = numTables * 16 - searchRange in
    let rawtbllst = List.sort compare_table rawtbllst in

(*
    Printf.printf "# make_font_file: header@,";
*)
  (* -- outputs the header -- *)
    let enc_header = create_encoder () in
    enc_uint32 enc_header sfntVersion   >>= fun () ->
    enc_uint16 enc_header numTables     >>= fun () ->
    enc_uint16 enc_header searchRange   >>= fun () ->
    enc_uint16 enc_header entrySelector >>= fun () ->
    enc_uint16 enc_header rangeShift    >>= fun () ->
    let data_header = Buffer.contents enc_header.buffer in
    let chksum_init = table_checksum data_header 12 in

    let enc = create_encoder () in
    enc_direct enc data_header >>= fun () ->

(*
    Printf.printf "# make_font_file: table directories@,";
*)
  (* -- outputs all table directories -- *)
    let offset_checkSumAdjustment_ref = ref None in
    let offset_init = 12 + numTables * 16 in
    rawtbllst |> List.fold_left (fun res rawtbl ->
      res >>= fun (offset, chksum) ->
      let strtag = Tag.to_bytes rawtbl.table_tag in
(*
      Printf.printf "## for '%s'@," strtag;
*)
      enc_direct enc strtag                             >>= fun () ->
      enc_uint32 enc rawtbl.table_checksum              >>= fun () ->
      enc_uint32 enc (!% offset)                        >>= fun () ->
      enc_uint32 enc (!% (rawtbl.table_content_length)) >>= fun () ->
      if strtag = "head" then
        offset_checkSumAdjustment_ref := Some(offset + 8)
      else
        ();
      let offsetnew = offset + rawtbl.table_padded_length in
      let chksumnew = add_checksum chksum rawtbl.table_checksum in
(*
      Printf.printf "  $ checksum %d + %d ---> %d@," chksum rawtbl.table_checksum chksumnew;
*)
      return (offsetnew, chksumnew)
    ) (return (offset_init, chksum_init)) >>= fun (_, chksum) ->

(*
    Printf.printf "# make_font_file: tables@,";
*)
  (* -- outputs all tables -- *)
    rawtbllst |> List.fold_left (fun res rawtbl ->
      res >>= fun () ->
      enc_direct enc rawtbl.table_data
    ) (return ()) >>= fun () ->

(*
    Printf.printf "# make_font_file: update 'checkSumAdjustment'@,";
*)
    let checkSumAdjustment =
      let temp = (!%% 0xB1B0AFBAL) -% chksum in
        if WideInt.is_neg temp then temp +% (!% (1 lsl 32)) else temp
    in
    match !offset_checkSumAdjustment_ref with
    | None ->
        err `Missing_head_table_for_encoding

    | Some(offset) ->
        let bytes = Buffer.to_bytes enc.buffer in
        let (b0, b1, b2, b3) = cut_uint32_unsafe checkSumAdjustment in
        Bytes.set bytes offset       b0;
        Bytes.set bytes (offset + 1) b1;
        Bytes.set bytes (offset + 2) b2;
        Bytes.set bytes (offset + 3) b3;
        let data = Bytes.to_string bytes in
        return data


  let empty_cmap () : raw_table ok =
    let enc = create_encoder () in
    enc_uint16 enc 0 >>= fun () ->  (* -- 'Version' -- *)
    enc_uint16 enc 0 >>= fun () ->  (* -- 'numTables' -- *)
    let rawtbl = to_raw_table Tag.cmap enc in
    return rawtbl


  let head (h : head) : raw_table ok =
(*
    Printf.printf "# 'head' table@,";
*)
    let enc = create_encoder () in
    let fontRevision = h.head_font_revision in
    let ilocfmt =
      match h.head_index_to_loc_format with
      | ShortLocFormat -> 0
      | LongLocFormat  -> 1
    in
    enc_uint32 enc (!% 0x00010000)     >>= fun () ->  (* -- Table Version Number -- *)
    enc_uint32 enc fontRevision        >>= fun () ->
    enc_uint32 enc (!% 0)              >>= fun () ->  (* -- 'checkSumAdjustment': will be updated afterwards -- *)
    enc_uint32 enc (!% 0x5F0F3CF5)     >>= fun () ->  (* -- 'magicNumber' -- *)
    enc_uint16 enc h.head_flags        >>= fun () ->
    enc_uint16 enc h.head_units_per_em >>= fun () ->
    enc_time   enc h.head_created      >>= fun () ->
    enc_time   enc h.head_modified     >>= fun () ->
    enc_int16  enc h.head_xmin         >>= fun () ->
    enc_int16  enc h.head_ymin         >>= fun () ->
    enc_int16  enc h.head_xmax         >>= fun () ->
    enc_int16  enc h.head_ymax         >>= fun () ->
    enc_uint16 enc h.head_mac_style    >>= fun () ->
    enc_uint16 enc h.head_lowest_rec_ppem     >>= fun () ->
    enc_int16  enc 0                          >>= fun () ->  (* -- 'fontDirectionHint' -- *)
    enc_int16  enc ilocfmt                    >>= fun () ->
    enc_int16  enc 0                          >>= fun () ->  (* -- 'glyphDataFormat' -- *)
    let rawtbl = to_raw_table Tag.head enc in
    return rawtbl


  let hhea (numberOfHMetrics : int) (h : hhea) : raw_table ok =
(*
    Printf.printf "# 'hhea' table@,";
*)
    let enc = create_encoder () in
    enc_uint32 enc (!% 0x00010000)               >>= fun () ->
    enc_int16  enc h.hhea_ascender               >>= fun () ->
    enc_int16  enc h.hhea_descender              >>= fun () ->
    enc_int16  enc h.hhea_line_gap               >>= fun () ->
    enc_uint16 enc h.hhea_advance_width_max      >>= fun () ->
    enc_int16  enc h.hhea_min_left_side_bearing  >>= fun () ->
    enc_int16  enc h.hhea_min_right_side_bearing >>= fun () ->
    enc_int16  enc h.hhea_xmax_extent            >>= fun () ->
    enc_int16  enc h.hhea_caret_slope_rise       >>= fun () ->
    enc_int16  enc h.hhea_caret_slope_run        >>= fun () ->
    enc_int16  enc h.hhea_caret_offset           >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->  (* -- 'metricDataFormat' -- *)
    enc_uint16 enc numberOfHMetrics              >>= fun () ->
(*
    Printf.printf "# end 'hhea' table@,";
*)
    let rawtbl = to_raw_table Tag.hhea enc in
    return rawtbl


  let maxp oltype (m : maxp) : raw_table ok =
(*
    Printf.printf "# 'maxp' table@,";
*)
    let enc = create_encoder () in
    ( match oltype with
      | TrueTypeOutline ->
          enc_uint32 enc (!% 0x00010000)                 >>= fun () ->  (* -- Table version number -- *)
          enc_uint16 enc m.maxp_num_glyphs               >>= fun () ->
          enc_uint16 enc m.maxp_max_points               >>= fun () ->
          enc_uint16 enc m.maxp_max_contours             >>= fun () ->
          enc_uint16 enc m.maxp_max_composite_points     >>= fun () ->
          enc_uint16 enc m.maxp_max_composite_contours   >>= fun () ->
          enc_uint16 enc m.maxp_max_zones                >>= fun () ->
          enc_uint16 enc m.maxp_max_twilight_points      >>= fun () ->
          enc_uint16 enc m.maxp_max_storage              >>= fun () ->
          enc_uint16 enc m.maxp_max_function_defs        >>= fun () ->
          enc_uint16 enc m.maxp_max_instruction_defs     >>= fun () ->
          enc_uint16 enc m.maxp_max_stack_elements       >>= fun () ->
          enc_uint16 enc m.maxp_max_size_of_instructions >>= fun () ->
          enc_uint16 enc m.maxp_max_component_elements   >>= fun () ->
          enc_uint16 enc m.maxp_max_component_depth

      | CFFData ->
          enc_uint32 enc (!% 0x00005000)                 >>= fun () ->  (* -- Table version number -- *)
          enc_uint16 enc m.maxp_num_glyphs

    ) >>= fun () ->
(*
    Printf.printf "# end 'maxp' table@,";
*)
    let rawtbl = to_raw_table Tag.maxp enc in
    return rawtbl


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
    | TrueTypeGlyph of raw_table * raw_table  (* -- glyf, loca -- *)
    | CFFGlyph      of raw_table              (* -- CFF -- *)


  let update_bbox (xMin0, yMin0, xMax0, yMax0) (xMin1, yMin1, xMax1, yMax1) =
    (min xMin0 xMin1, min yMin0 yMin1, max xMax0 xMax1, max yMax0 yMax1)


  let get_right_side_bearing g =
    let (xmin, _, xmax, _) = g.glyph_bbox in
    g.glyph_aw - (g.glyph_lsb + xmax - xmin)


  let get_x_extent g =
    let (xmin, _, xmax, _) = g.glyph_bbox in
    g.glyph_lsb + (xmax - xmin)


  module OldToNewGlyphID = Hashtbl.Make
    (struct
      type t = glyph_id
      let equal = ( = )
      let hash = Hashtbl.hash
    end)


  let fix_composite_element_glyph_id (ht : glyph_id OldToNewGlyphID.t) (g : raw_glyph) : string =
    let bytes = Bytes.of_string g.glyph_data in
    let pairlst = g.glyph_composite_offsets in
    pairlst |> List.iter (fun (oldgid, offset) ->
      match OldToNewGlyphID.find_opt ht oldgid with
      | None         -> set_uint16 bytes offset 0
      | Some(newgid) -> set_uint16 bytes offset newgid
    );
    Bytes.to_string bytes



  let ttf_outline_tables (glyphlst : OtfDecTTF.ttf_raw_glyph list) : (glyph_output_info * glyph_data) ok =
    let glyphlst = glyphlst |> List.map OtfDecTTF.ttf_raw_glyph in
(*
    Printf.printf "# 'hmtx', 'glyf', and 'loca' table@,";
*)

    let numGlyphs = List.length glyphlst in

    let ht = OldToNewGlyphID.create (numGlyphs * 2) in
    glyphlst |> List.iteri (fun gidnew rg ->
      OldToNewGlyphID.add ht rg.old_glyph_id gidnew
    );

    if numGlyphs > 65536 then
      err (`Too_many_glyphs_for_encoding(numGlyphs))
    else
      begin
        match glyphlst with
        | []          -> err `No_glyph_for_encoding
        | gfirst :: _ -> return gfirst
      end >>= fun gfirst ->
      let lsb_init = gfirst.glyph_lsb in
      let rsb_init = get_right_side_bearing gfirst in
      let xext_init = get_x_extent gfirst in

      let numberOfHMetrics = numGlyphs in

    (* -- outputs 'hmtx' table and calculates (xMin, yMin, xMax, yMax) -- *)
      let enc_hmtx = create_encoder () in
      let bbox_init = (0, 0, 0, 0) in
      let aw_init = 0 in
      glyphlst |> List.fold_left (fun res g ->
        res >>= fun (bbox, aw, lsb, rsb, xext) ->
        enc_uint16 enc_hmtx g.glyph_aw  >>= fun () ->
        enc_int16  enc_hmtx g.glyph_lsb >>= fun () ->
        let bboxnew = update_bbox bbox g.glyph_bbox in
        let awnew = max aw g.glyph_aw in
        let lsbnew = min lsb g.glyph_lsb in
        let rsbnew = min rsb (get_right_side_bearing g) in
        let xextnew = max xext (get_x_extent g) in
        return (bboxnew, awnew, lsbnew, rsbnew, xextnew)
      ) (return (bbox_init, aw_init, lsb_init, rsb_init, xext_init))
        >>= fun ((xMin, yMin, xMax, yMax), awmax, minlsb, minrsb, xmaxext) ->
      let rawtbl_hmtx = to_raw_table Tag.hmtx enc_hmtx in

    (* -- outputs 'glyf' table and 'loca' table -- *)
      let enc_glyf = create_encoder () in
      let enc_loca = create_encoder () in
      let lenwhole =
        glyphlst |> List.fold_left (fun acc g -> acc + g.glyph_data_length) 0
      in
      let (locfmt, enc_for_loca) =
        if lenwhole <= 2 * (1 lsl 16) then
          (ShortLocFormat, (fun offset -> enc_uint16 enc_loca (offset / 2)))
        else
          (LongLocFormat, (fun offset -> enc_uint32 enc_loca (!% offset)))
      in
      let offset_init = 0 in
      glyphlst |> List.fold_left (fun res g ->
        res >>= fun offset ->
        let data = fix_composite_element_glyph_id ht g in
        let len = g.glyph_data_length in
        let plen = (4 - (len mod 4)) mod 4 in
        enc_direct enc_glyf data >>= fun () ->
        enc_pad enc_glyf plen >>= fun () ->
        enc_for_loca offset >>= fun () ->
        return (offset + len + plen)
      ) (return offset_init) >>= fun offset_last ->
      enc_for_loca offset_last >>= fun () ->
      let rawtbl_glyf = to_raw_table Tag.glyf enc_glyf in
      let rawtbl_loca = to_raw_table Tag.loca enc_loca in

      let out_info =
        {
          hmtx = rawtbl_hmtx;

          number_of_glyphs = numGlyphs;

          xmin = xMin;
          ymin = yMin;
          xmax = xMax;
          ymax = yMax;
          index_to_loc_format = locfmt;

          advance_width_max      = awmax;
          min_left_side_bearing  = minlsb;
          min_right_side_bearing = minrsb;
          x_max_extent           = xmaxext;
          number_of_h_metrics    = numberOfHMetrics;
        }
      in
      let glyph_data = TrueTypeGlyph(rawtbl_glyf, rawtbl_loca) in
      return (out_info, glyph_data)


  let get_charstring_length = function
    CharStringData(_, len) -> len


  let calculate_subr_index_length =
    calculate_index_length_of_array get_charstring_length


  let fix_charstrings_offset zoffset dictmap =
    dictmap |> DictMap.update (ShortKey(17)) (function _ -> Some([Integer(zoffset)]))


  let fix_private_offset len zoffset dictmap =
    dictmap |> DictMap.update (ShortKey(18)) (function _ -> Some([Integer(len); Integer(zoffset)]))


  let fix_lsubr_offset selfoffset dictmap =
    dictmap |> DictMap.update (ShortKey(19)) (fun _ -> Some([Integer(selfoffset)]))


  let fix_charset_offset zoffset dictmap =
    dictmap |> DictMap.update (ShortKey(15)) (fun _ -> Some([Integer(zoffset)]))


  let fix_fd_related_offset zoffset_fdidx zoffset_fdsel dictmap =
    dictmap |> DictMap.update (LongKey(36)) (fun _ -> Some([Integer(zoffset_fdidx)]))
            |> DictMap.update (LongKey(37)) (fun _ -> Some([Integer(zoffset_fdsel)]))


  let make_elem_len_pair_of_array (type a) (lenf : a -> int) (arr : a array) : (a * int) array =
    Array.map (fun e -> (e, lenf e)) arr


  let enc_charstring_data d enc csd =
    match csd with
    | CharStringData(offset, len) ->
        enc_copy_direct d enc offset len


  type subrs_index_location =
    | Global                  (* -- Global Subrs -- *)
    | LocalInTopDict          (* -- in the Local Subrs in the Private DICT -- *)
    | LocalInFontDict of int  (* -- in a Local Subrs in an element of FDArray -- *)


  module SubrsIndexMap = Map.Make
    (struct
      type t = subrs_index_location
      let compare i j = Pervasives.compare i j
    end)


  let enc_cff_table (dcff : OtfDecCFF.cff_decoder) (enc : encoder) (glyphlst : raw_glyph list) =
    let open OtfDecCFF in
    let d = cff_common dcff in
    OtfDecCFF.cff dcff >>= fun cff_top ->
    let cff_first = cff_top.cff_first in
    let header       = cff_first.cff_header in
    let name         = cff_first.cff_name in
    let dict_Top     = cff_first.top_dict in
    let stridx       = cff_first.string_index in
    let gsubridx_org = cff_first.gsubr_index in
    let offset_CFF   = cff_first.offset_CFF in

    let numGlyphs = List.length glyphlst in
    let glypharr = Array.of_list glyphlst in
    OtfDecCFF.cff_private_info dcff >>= fun privinfo ->
    begin
      match privinfo with
      | SinglePrivate(singlepriv) ->
          return None

      | FontDicts(_, fdselect) ->
          let fdmapping = Array.make numGlyphs 0 in
          let regf gid_sub gid_org =
            select_fd_index fdselect gid_org >>= fun fdindex ->
            fdmapping.(gid_sub) <- fdindex;
            return ()
          in
          glyphlst |> List.fold_left (fun res g ->
            res >>= fun gid_sub ->
            regf gid_sub g.old_glyph_id >>= fun () ->
            return (gid_sub + 1)
          ) (return 0) >>= fun _ ->
          return (Some(fdmapping))

    end >>= fun fdmappingopt ->

    let dict_Top = dict_Top |> fix_charset_offset 0 (* dummy *) in (* allocate charset entry *)

    glypharr |> Array.fold_left (fun acc rg ->
      acc >>= fun usmap ->
      match rg.glyph_data_offset with
      | None ->
          return usmap

      | Some(glyph_offset) ->
          let cstate = { numarg = 0; numstem = 0; used_gsubr_set = IntSet.empty; used_lsubr_set = IntSet.empty; } in
          let stk : int Stack.t = Stack.create () in
          let extend_set s = (function None -> Some(s) | Some(s0) -> Some(IntSet.union s0 s)) in
          begin
            match privinfo with
            | SinglePrivate(_) ->
                return LocalInTopDict

            | FontDicts(_, fdselect) ->
                select_fd_index fdselect rg.old_glyph_id >>= fun i ->
                return (LocalInFontDict(i))
          end >>= fun lsubrloc ->
          select_local_subr_index privinfo rg.old_glyph_id >>= fun lsubridx ->
          seek_pos glyph_offset d >>= fun () ->
          parse_charstring rg.glyph_data_length cstate d stk gsubridx_org lsubridx WidthLookedFor >>= function
          | (_, cstate, _) ->
              return (usmap |> SubrsIndexMap.update Global   (extend_set cstate.used_gsubr_set)
                            |> SubrsIndexMap.update lsubrloc (extend_set cstate.used_lsubr_set))

    ) (return SubrsIndexMap.empty) >>= fun (used_subrs_map : IntSet.t SubrsIndexMap.t) ->

    let remove_unused_subrs subrloc subridx =
      match used_subrs_map |> SubrsIndexMap.find_opt subrloc with
      | None ->
          subridx

      | Some(used_set) ->
          subridx |> Array.mapi (fun i (CharStringData(offset, len)) ->
            if IntSet.mem i used_set then
              (CharStringData(offset, len))
            else
              (CharStringData(offset + len - 1, 1))
                (* --
                   just sets to 1 the designation of the data length
                   and does not modify the Subrs INDEX data itself;
                   the modification of the Subrs INDEX
                   is performed by `enc_charstring_data`.
                   (NOTE: some applications require that
                            the data length is more than 0.)
                   -- *)
          )
    in
    let gsubridx_sub = remove_unused_subrs Global gsubridx_org in

    let len_Header     = header.hdrSize in
    let len_Name_INDEX = calculate_index_length 1 (String.length name) in
    let len_Top_DICT_INDEX = calculate_index_length 1 (calculate_encoded_dict_length ~is_fixed_int:true dict_Top) in
    let len_String_INDEX = calculate_index_length_of_array String.length stridx in
    let len_GSubrs_INDEX = calculate_subr_index_length gsubridx_sub in
    let zoffset_Charset = len_Header + len_Name_INDEX + len_Top_DICT_INDEX + len_String_INDEX + len_GSubrs_INDEX in
    let len_Charset = 1 + 2 + 2 (* Format 2 *) in
    let zoffset_FDSelect = zoffset_Charset + len_Charset in
    let len_FDSelect =
      match fdmappingopt with
      | None            -> 0
      | Some(fdmapping) -> 1 + Array.length fdmapping (* Format0 *)
    in
    let zoffset_Charstrings_INDEX = zoffset_FDSelect + len_FDSelect in
    let len_Charstrings_INDEX = calculate_index_length_of_array (fun rg -> rg.glyph_data_length) glypharr in
    let zoffset_Font_DICT_INDEX = zoffset_Charstrings_INDEX + len_Charstrings_INDEX in

    let dict_Top = dict_Top |> fix_charstrings_offset zoffset_Charstrings_INDEX
                            |> fix_charset_offset zoffset_Charset
    in

    (* layout remaining data, and fix offsets in DICTs *)
    begin
      match fdmappingopt with
      | None ->
          d_private_lsubr_pair_opt offset_CFF dict_Top d >>= fun pairopt ->
          begin
            match pairopt with
            | None ->
              (* -- if the original Top DICT does NOT have `Private` entry -- *)
                return (dict_Top, None, [| |], [| |], [| |])

            | Some(dict_Private, lsubropt) ->
                let lsubrarray =
                  match lsubropt with
                  | None        -> [| |]
                  | Some(lsubr) -> [| remove_unused_subrs LocalInTopDict lsubr |]
                in
                let len_Private = calculate_encoded_dict_length ~is_fixed_int:true dict_Private in
                let zoffset_Private = zoffset_Font_DICT_INDEX + 0 in
                let zoffset_lsubr_start = zoffset_Private + len_Private in
                let dict_Top = dict_Top |> fix_private_offset len_Private zoffset_Private in
                let selfoffset_lsubr = zoffset_lsubr_start - zoffset_Private in
                let dict_Private = dict_Private |> fix_lsubr_offset selfoffset_lsubr in
                return (dict_Top, None, [| |], [| dict_Private |], lsubrarray)
          end

      | Some(fdmapping) ->

          let extract_pairarray (pairarray : (dict * (dict * subroutine_index option) option) array) : dict array * dict array * subroutine_index array =
            let fdarray = pairarray |> Array.map fst in
            let (privacc, lsubracc) =
              pairarray |> Array.fold_left (fun (pacc, lacc) (_, pairopt) ->
                match pairopt with
                | None             -> (pacc, lacc)
                | Some(p, None)    -> (Alist.extend pacc p, lacc)
                | Some(p, Some(l)) -> (Alist.extend pacc p, Alist.extend lacc l)
              ) (Alist.empty, Alist.empty)
            in
            let privarray  = privacc |> Alist.to_list |> Array.of_list in
            let lsubrarray = lsubracc |> Alist.to_list |> Array.of_list in
            (fdarray, privarray, lsubrarray)
          in

          (* --
             `remove_unused_fontdict`: receives

             * `fdmapping`: the array of original FD indices indexed by subset GIDs, and
             * `fdpairs_org`: the original Font DICTs and their corresponding Private DICTs and Local Subrs INDEXes,

             and then returns `(fdmapping_sub, revmap, fdarr_sub)` where:

             * `fdmapping_sub`: the renumbered version of `fdmapping_org`,
             * `revmap`: a mapping from subset FD indices to original ones, and
             * `fdpairs_sub`: necessary subset of `fdarr`.
             -- *)
          let remove_unused_fontdict (fdmapping : fdindex array) (fdpairs_org : (dict * (dict * subroutine_index option) option) array) : fdindex array * fdindex IntMap.t * (dict * (dict * subroutine_index option) option) array =
            let usedset = fdmapping |> Array.fold_left (fun set fdi -> IntSet.add fdi set) IntSet.empty in
            let (_, _, map, revmap, fdpairacc) =
              fdpairs_org |> Array.fold_left (fun (fdi_org, fdi_sub, map, revmap, fdpairacc) fdpair ->
                if IntSet.mem fdi_org usedset then
                  (fdi_org + 1, fdi_sub + 1, map |> IntMap.add fdi_org fdi_sub, revmap |> IntMap.add fdi_sub fdi_org, Alist.extend fdpairacc fdpair)
                else
                  (fdi_org + 1, fdi_sub, map, revmap, fdpairacc)
              ) (0, 0, IntMap.empty, IntMap.empty, Alist.empty)
            in
            let fdmapping_sub =
              fdmapping |> Array.map (fun fdi_org ->
                match map |> IntMap.find_opt fdi_org with
                | None          -> fdi_org
                | Some(fdi_sub) -> fdi_sub
              )
            in
            let fdpairs_sub = fdpairacc |> Alist.to_list |> Array.of_list in
            (fdmapping_sub, revmap, fdpairs_sub)
          in

          d_fontdict_private_pair_array offset_CFF dict_Top d >>= fun fdpairs_org ->
          let (fdmapping_sub, fdnew2oldmap, fdpairs_sub) = fdpairs_org |> remove_unused_fontdict fdmapping in
          let (fdarray, privarray, _) = extract_pairarray fdpairs_sub in

          let len_Font_DICT_INDEX = calculate_index_length_of_array (calculate_encoded_dict_length ~is_fixed_int:true) fdarray in
          let len_Private = sum_of_array (calculate_encoded_dict_length ~is_fixed_int:true) privarray in
          let zoffset_Private  = zoffset_Font_DICT_INDEX + len_Font_DICT_INDEX in
          let zoffset_LSubrs_INDEX = zoffset_Private + len_Private in

          fdpairs_sub |> Array.fold_left (fun (i, offset_priv_next, offset_lsubr_next) fdpair ->
            match fdpair with
            | (fd, Some(priv, lsubropt)) ->
                let len_thispriv = (calculate_encoded_dict_length ~is_fixed_int:true) priv in
                let fdnew = fix_private_offset len_thispriv offset_priv_next fd in
                let (privnew, len_thislsubr, lsubropt_reduced) =
                  match lsubropt with
                  | Some(lsubr) ->
                      let lsubr = remove_unused_subrs (LocalInFontDict(fdnew2oldmap |> IntMap.find i)) lsubr in
                      (fix_lsubr_offset (offset_lsubr_next - offset_priv_next) priv, calculate_subr_index_length lsubr, Some(lsubr))

                  | None ->
                      (priv, 0, None)
                in
                let pairnew = (fdnew, Some(privnew, lsubropt_reduced)) in
                fdpairs_sub.(i) <- pairnew;
                (i + 1, offset_priv_next + len_thispriv, offset_lsubr_next + len_thislsubr)

            | (_, None) ->
                (i + 1, offset_priv_next, offset_lsubr_next)

          ) (0, zoffset_Private, zoffset_LSubrs_INDEX) |> ignore;

          let (fdarraynew, privarraynew, lsubrarraynew) = extract_pairarray fdpairs_sub in
          let dict_Top = dict_Top |> fix_fd_related_offset zoffset_Font_DICT_INDEX zoffset_FDSelect in
          return (dict_Top, Some(fdmapping_sub), fdarraynew, privarraynew, lsubrarraynew)

    end >>= fun (dictmap, fdmappingopt, fdarray, privarray, lsubrarray) ->

    (* -- Header: -- *)
    enc_copy_direct     d enc offset_CFF len_Header >>= fun () ->
    (* -- Name INDEX: -- *)
    enc_index_singleton enc enc_direct (String.length name) name >>= fun () ->
    (* -- Top DICT INDEX: -- *)
    enc_index_singleton enc (enc_dict ~is_fixed_int:true) (calculate_encoded_dict_length ~is_fixed_int:true dictmap) dictmap >>= fun () ->
    (* -- String INDEX: -- *)
    enc_index           enc enc_direct (make_elem_len_pair_of_array String.length stridx) >>= fun () ->
    (* -- Global Subr INDEX: -- *)
    enc_index           enc (enc_charstring_data d)
                          (make_elem_len_pair_of_array get_charstring_length gsubridx_sub) >>= fun () ->
    (* -- Charsets: -- *)
    enc_charset_identity enc numGlyphs >>= fun () ->
    (* -- FDSelect (CIDFonts only): -- *)
    begin
      match fdmappingopt with
      | None            -> return ()
      | Some(fdmapping) -> enc_fdselect_format0 enc fdmapping
    end >>= fun () ->
    (* -- CharStrings INDEX: -- *)
    enc_index enc enc_direct (Array.map (fun rg -> (rg.glyph_data, rg.glyph_data_length)) glypharr) >>= fun () ->
    (* -- Font DICT INDEX (CIDFonts only): -- *)
    begin
      match fdmappingopt with
      | None      -> return ()
      | Some(_)   -> enc_index enc (enc_dict ~is_fixed_int:true) (make_elem_len_pair_of_array (calculate_encoded_dict_length ~is_fixed_int:true) fdarray)
    end >>= fun () ->
    (* -- Private DICT: -- *)
    enc_array enc (enc_dict ~is_fixed_int:true) privarray >>= fun () ->
    (* -- Local Subr INDEX: -- *)
    enc_array enc (fun enc idx -> enc_index enc (enc_charstring_data d)
                    (make_elem_len_pair_of_array get_charstring_length idx)) lsubrarray


  let cff_outline_tables (dcff : cff_decoder) (glyphlst : OtfDecCFF.cff_raw_glyph list) =
    let glyphlst = glyphlst |> List.map OtfDecCFF.cff_raw_glyph in
    let open OtfDecCFF in
    cff dcff >>= fun cffinfo ->

    let numGlyphs = List.length glyphlst in
    if numGlyphs > 65536 then
      err (`Too_many_glyphs_for_encoding(numGlyphs))
    else
      begin
        match glyphlst with
        | []          -> err `No_glyph_for_encoding
        | gfirst :: _ -> return gfirst
      end >>= fun gfirst ->
      let lsb_init = gfirst.glyph_lsb in
      let rsb_init = get_right_side_bearing gfirst in
      let xext_init = get_x_extent gfirst in

      let numberOfHMetrics = numGlyphs in

    (* -- outputs 'hmtx' table and calculates (xMin, yMin, xMax, yMax) -- *)
      let enc_hmtx = create_encoder () in
      let bbox_init = (0, 0, 0, 0) in
      let aw_init = 0 in
      glyphlst |> List.fold_left (fun res g ->
        res >>= fun (bbox, aw, lsb, rsb, xext) ->
        enc_uint16 enc_hmtx g.glyph_aw  >>= fun () ->
        enc_int16  enc_hmtx g.glyph_lsb >>= fun () ->
        let bboxnew = update_bbox bbox g.glyph_bbox in
        let awnew = max aw g.glyph_aw in
        let lsbnew = min lsb g.glyph_lsb in
        let rsbnew = min rsb (get_right_side_bearing g) in
        let xextnew = max xext (get_x_extent g) in
        return (bboxnew, awnew, lsbnew, rsbnew, xextnew)
      ) (return (bbox_init, aw_init, lsb_init, rsb_init, xext_init))
        >>= fun ((xMin, yMin, xMax, yMax), awmax, minlsb, minrsb, xmaxext) ->
      let rawtbl_hmtx = to_raw_table Tag.hmtx enc_hmtx in

    (* -- outputs 'cff' table -- *)
      let enc_cff = create_encoder () in
      enc_cff_table dcff enc_cff glyphlst >>= fun () ->
      let rawtbl_cff = to_raw_table Tag.cff enc_cff in

      let out_info =
        {
          hmtx = rawtbl_hmtx;

          number_of_glyphs = numGlyphs;

          xmin = xMin;
          ymin = yMin;
          xmax = xMax;
          ymax = yMax;
          index_to_loc_format = ShortLocFormat; (* dummy *)

          advance_width_max      = awmax;
          min_left_side_bearing  = minlsb;
          min_right_side_bearing = minrsb;
          x_max_extent           = xmaxext;
          number_of_h_metrics    = numberOfHMetrics;
        }
      in
      let glyph_data = CFFGlyph(rawtbl_cff) in
      return (out_info, glyph_data)


end
