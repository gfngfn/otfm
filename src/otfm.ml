(* -*- coding: utf-8 -*- *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

(* Error strings *)

let err_invalid_tag s = Printf.sprintf "invalid OpenType tag (%S)" s

(* Unsafe string byte manipulations.

   If you don't believe the author's invariants, replacing with safe
   versions makes everything safe in the module. He won't be
   upset. *)

let unsafe_chr = Char.unsafe_chr
let unsafe_byte s j = Char.code (String.unsafe_get s j)

(* Pretty printers *)

let pp = Format.fprintf
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

(* OpenType tags *)

type tag = int32

module Tag = struct
  type t = tag

  (* OpenType version tags. *)

  let v_wOFF = 0x774F4646l
  let v_OTTO = 0x4F54544Fl
  let v_ttcf = 0x74746366l
  let v_true = 0x74727565l (* may happen in the wild. *)

  (* Required common tables tags *)

  let cmap = 0x636D6170l
  let head = 0x68656164l
  let hhea = 0x68686561l
  let hmtx = 0x686D7478l
  let maxp = 0x6D617870l
  let name = 0x6E616D65l
  let os2  = 0x4F532F32l
  let post = 0x706F7374l

  let t_common = [ cmap; head; hhea; hmtx; maxp; name; os2; post ]

  (* TTF font table tags *)

  let cvt  = 0x63767420l
  let fpgm = 0x6670676Dl
  let glyf = 0x676C7966l
  let loca = 0x6C6F6361l
  let prep = 0x70726570l

  (* CFF font table tags *)

  let cff  = 0x43464620l
  let vorg = 0x564F5247l

  (* Bitmap glyph tables *)

  let ebdt = 0x45424454l
  let eblc = 0x45424C43l
  let ebsc = 0x45425343l

  (* Optional tables. *)

  let dsig = 0x44534947l
  let gasp = 0x67617370l
  let hdmx = 0x68646D78l
  let kern = 0x6B65726El
  let ltsh = 0x4C545348l
  let pclt = 0x50434C54l
  let vdmx = 0x56444D58l
  let vhea = 0x76686561l
  let vmtx = 0x766D7478l

  (* Advanced Open Type font layout tables *)

  let base = 0x42415345l
  let gdef = 0x47444546l
  let gpos = 0x47504F53l
  let gsub = 0x47535542l
  let jstf = 0x4A535446l

  (* Functions *)

  let of_bytes s =
    if String.length s <> 4 then invalid_arg (err_invalid_tag s) else
    let s0 = Int32.of_int ((Char.code s.[0] lsl 8) lor (Char.code s.[1])) in
    let s1 = Int32.of_int ((Char.code s.[2] lsl 8) lor (Char.code s.[3])) in
    Int32.(logor (shift_left s0 16) s1)

  let to_bytes t =
    let c0 = Char.chr (Int32.(to_int (shift_right t 24))) in
    let c1 = Char.chr (Int32.(to_int (shift_right t 16)) land 0xFF) in
    let c2 = Char.chr (Int32.(to_int (shift_right t 8)) land 0xFF) in
    let c3 = Char.chr (Int32.(to_int t) land 0xFF) in
    Printf.sprintf "%c%c%c%c" c0 c1 c2 c3

  let to_int32 x = x
  let of_int32 x = x
  let compare : int32 -> int32 -> int = Pervasives.compare
  let pp ppf t = pp ppf "'%s'" (to_bytes t)
end

(* Unicode code points *)

type cp = int
type cp_range = cp * cp
let is_cp i = 0x0000 <= i && i <= 0x10FFFF
let pp_cp ppf cp = Format.fprintf ppf "U+%04X" cp

(* Decode *)

type error_ctx = [ `Table of tag | `Offset_table | `Table_directory ]
type error =
[
  | `Unknown_flavour of tag
  | `Unsupported_TTC
  | `Unsupported_cmaps of (int * int * int) list
  | `Unsupported_glyf_matching_points
  | `Missing_required_table of tag
  | `Unknown_version of error_ctx * int32
  | `Unknown_loca_format of error_ctx * int
  | `Unknown_composite_format of error_ctx * int
  | `Invalid_offset of error_ctx * int
  | `Invalid_cp of int
  | `Invalid_cp_range of int * int
  | `Invalid_postscript_name of string
  | `Unexpected_eoi of error_ctx
(* added by gfn: *)
  | `Inconsistent_length_of_coverage of error_ctx
  | `Inconsistent_length_of_class
  | `Missing_required_script_tag of string
  | `Missing_required_langsys_tag of string
  | `Missing_required_feature_tag of string
  | `Invalid_lookup_order of int
  | `Invalid_extension_position
  | `Invalid_cff_not_a_quad
  | `Invalid_cff_not_an_integer
  | `Invalid_cff_not_an_element
  | `Invalid_cff_not_an_offsize of int
  | `Invalid_cff_not_a_singleton
  | `Invalid_cff_inconsistent_length
  | `Invalid_cff_invalid_first_offset
]

let pp_ctx ppf = function
| `Table tag -> pp ppf "table %a" Tag.pp tag
| `Offset_table -> pp ppf "offset table"
| `Table_directory -> pp ppf "table directory"

let pp_error ppf = function
| `Unknown_flavour tag ->
    pp ppf "@[Unknown@ OpenType@ flavour (%a)@]" Tag.pp tag
| `Missing_required_table tag ->
    pp ppf "@[Missing@ required@ table (%a)@]" Tag.pp tag
| `Unsupported_TTC ->
    pp ppf "@[True@ Type@ collections (TTC)@ are@ not@ supported@]"
| `Unsupported_cmaps maps ->
    let pp_sep ppf () = pp ppf ",@ " in
    let pp_map ppf (pid, eid, fmt) = pp ppf "(%d,%d,%d)" pid eid fmt in
    pp ppf "@[All@ cmaps:@ %a@ are@ unsupported@]" (pp_list ~pp_sep pp_map) maps
| `Unsupported_glyf_matching_points ->
    pp ppf "@[Unsupported@ glyf@ matching@ points)@]"
| `Unknown_version (ctx, v) ->
    pp ppf "@[Unknown@ version (%lX)@ in@ %a@]" v pp_ctx ctx
| `Unknown_loca_format (ctx, v) ->
    pp ppf "@[Unknown@ loca table format (%d)@ in@ %a@]" v pp_ctx ctx
| `Unknown_composite_format (ctx, v) ->
    pp ppf "@[Unknown@ composite glyph format (%d)@ in@ %a@]" v pp_ctx ctx
| `Invalid_offset (ctx, o) ->
    pp ppf "@[Invalid@ offset (%d)@ in@ %a@]" o pp_ctx ctx
| `Invalid_cp u ->
    pp ppf "@[Invalid@ Unicode@ code@ point@ (%a)@]" pp_cp u
| `Invalid_cp_range (u0, u1) ->
    pp ppf "@[Invalid@ Unicode@ code@ point@ range (%a, %a)@]" pp_cp u0 pp_cp u1
| `Invalid_postscript_name n ->
    pp ppf "@[Invalid@ PostScript@ name (%S)@]" n
| `Unexpected_eoi ctx ->
    pp ppf "@[Unexpected@ end@ of@ input@ in %a@]" pp_ctx ctx
(* added by gfn: *)
| `Inconsistent_length_of_coverage ctx ->
    pp ppf "@[Inconsistent@ length@ of@ coverage@ in %a@]" pp_ctx ctx
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
| `Invalid_extension_position ->
    pp ppf "@[Invalid@ extension@ position@]"
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
| `Invalid_cff_inconsistent_length ->
    pp ppf "@[Invalid@ CFF@ table;@ inconsistent@ length@]"
| `Invalid_cff_invalid_first_offset ->
    pp ppf "@[Invalid@ CFF@ table;@ invalid@ first@ offset@]"
(* N.B. Offsets and lengths are decoded as OCaml ints. On 64 bits
   platforms they fit, on 32 bits we are limited by string size
   anyway. *)

type flavour = [ `TTF_true | `TTF_OT | `CFF ]
type src = [ `String of string ]

(* TODO maybe it would be better not to maintain t_pos/i_pos,
   but rather pass them as arguments to decoding functions. *)

type decoder =
  {
    mutable i : string;                                       (* input data. *)
    mutable i_pos : int;                          (* input current position. *)
    mutable i_max : int;                          (* input maximal position. *)
    mutable t_pos : int;                  (* current decoded table position. *)
    mutable state : [ `Fatal of error | `Start | `Ready ]; (* decoder state. *)
    mutable ctx : error_ctx;                   (* the current error context. *)
    mutable flavour : flavour;                           (* decoded flavour. *)
    mutable tables : (tag * int * int) list;       (* decoded table records. *)
    mutable loca_pos : int;                    (* for `TTF fonts, lazy init. *)
    mutable loca_format : int;                 (* for `TTF fonts, lazy init. *)
    mutable glyf_pos : int;                    (* for `TTF fonts, lazy init. *)
    mutable buf : Buffer.t;                              (* internal buffer. *)
  }

let decoder_src d = `String(d.i)
let decoder src =
  let (i, i_pos, i_max) =
    match src with
    | `String(s) -> (s, 0, String.length s - 1)
  in
  { i; i_pos; i_max; t_pos = 0;
    state = `Start; ctx = `Offset_table; flavour = `TTF_OT; tables = [];
    loca_pos = -1; loca_format = -1; glyf_pos = -1;
    buf = Buffer.create 253; }

let ( >>= ) x f = match x with Ok(v) -> f v | Error(_) as e -> e
let return x                 = Ok(x)
let err e                    = Error(e)

let err_eoi d                = err (`Unexpected_eoi(d.ctx))
let e_version d v            = `Unknown_version(d.ctx, v)
let err_version d v          = err (e_version d v)
let err_loca_format d v      = err (`Unknown_loca_format(d.ctx, v))
let err_composite_format d v = err (`Unknown_composite_format(d.ctx, v))
let err_fatal d e            = begin d.state <- `Fatal(e); err e end
let set_ctx d ctx            = begin d.ctx <- ctx; end

let miss d count = d.i_max - d.i_pos + 1 < count
let cur_pos d = d.i_pos
let seek_pos pos d =
  if pos > d.i_max then err (`Invalid_offset(d.ctx, pos)) else
    begin d.i_pos <- pos; return () end

let seek_table_pos pos d = seek_pos (d.t_pos + pos) d
let seek_table tag d () =
  try
    let (_, pos, len) = List.find (fun (t, _, _) -> tag = t) d.tables in
      if pos > d.i_max then err (`Invalid_offset(`Table(tag), pos)) else
        begin
          set_ctx d (`Table(tag));
          d.t_pos <- pos;
          d.i_pos <- pos;
          return (Some(len))
        end
  with
  | Not_found -> Ok(None)

let seek_required_table tag d () =
  seek_table tag d () >>= function
    | Some(_) -> return ()
    | None    -> err (`Missing_required_table(tag))

let d_skip len d =
  if miss d len then err_eoi d else
    begin d.i_pos <- d.i_pos + len; Ok() end

let raw_byte d =
  let j = d.i_pos in
    begin d.i_pos <- d.i_pos + 1; unsafe_byte d.i j end

let d_bytes len d =
  if miss d len then err_eoi d else
    let start = d.i_pos in
      begin d.i_pos <- d.i_pos + len; return (String.sub d.i start len) end

let d_uint8 d = if miss d 1 then err_eoi d else return (raw_byte d)
let d_int8 d =
  d_uint8 d >>= fun i ->
  return (if i > 0x7F then i - 0x100 else i)

let d_uint16 d =
  if miss d 2 then err_eoi d else
    let b0 = raw_byte d in
    let b1 = raw_byte d in
    return ((b0 lsl 8) lor b1)

let d_int16 d =
  d_uint16 d >>= fun i ->
  return (if i > 0x7FFF then i - 0x10000 else i)

let d_uint24 d =
  if miss d 3 then err_eoi d else
    let b0 = raw_byte d in
    let b1 = raw_byte d in
    let b2 = raw_byte d in
    return ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let d_uint32 d =
  if miss d 4 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let s0 = Int32.of_int ((b0 lsl 8) lor b1) in
    let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
    return (Int32.logor (Int32.shift_left s0 16) s1)

let d_uint32_int d =
  if miss d 4 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let s0 = (b0 lsl 8) lor b1 in
    let s1 = (b2 lsl 8) lor b3 in
    return ((s0 lsl 16) lor s1)

let d_time d =                       (* LONGDATETIME as a unix time stamp. *)
  if miss d 8 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let b4 = raw_byte d in let b5 = raw_byte d in
    let b6 = raw_byte d in let b7 = raw_byte d in
    let s0 = Int64.of_int ((b0 lsl 8) lor b1) in
    let s1 = Int64.of_int ((b2 lsl 8) lor b3) in
    let s2 = Int64.of_int ((b4 lsl 8) lor b5) in
    let s3 = Int64.of_int ((b6 lsl 8) lor b7) in
    let v = (Int64.logor (Int64.shift_left s0 48)
               (Int64.logor (Int64.shift_left s1 32)
                  (Int64.logor (Int64.shift_left s2 16) s3)))
    in
    let unix_epoch = 2_082_844_800L (* in seconds since 1904-01-01 00:00:00 *) in
    return (Int64.to_float (Int64.sub v unix_epoch))

let d_fixed d =
  if miss d 4 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let s0 = Int32.of_int ((b0 lsl 8) lor b1) in
    let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
    return (s0, s1)

let d_f2dot14 d =
  d_int16 d >>= fun v ->
  return ((float v) /. 16384.0)

let d_utf_16be len (* in bytes *) d =            (* returns an UTF-8 string. *)
  let rec add_utf_8 b i = function
    | `Malformed(_) -> add_utf_8 b i (`Uchar(Uutf.u_rep))
    | `Uchar(u)     -> begin Uutf.Buffer.add_utf_8 b u; b end
  in
  d_bytes len d >>= fun s ->
  Buffer.clear d.buf;
  return (Buffer.contents (Uutf.String.fold_utf_16be add_utf_8 d.buf s))

let rec d_table_records d count =
  if count = 0 then begin d.state <- `Ready; return () end else
    d_uint32     d >>= fun tag ->
    d_skip 4     d >>= fun () ->
    d_uint32_int d >>= fun off ->
    d_uint32_int d >>= fun len ->
    d.tables <- (tag, off, len) :: d.tables;
    d_table_records d (count - 1)

let d_version d =
  d_uint32 d >>= function
    | t  when t = Tag.v_OTTO  -> begin d.flavour <- `CFF     ; return () end
    | t  when t = Tag.v_true  -> begin d.flavour <- `TTF_true; return () end
    | t  when t = 0x00010000l -> begin d.flavour <- `TTF_OT  ; return () end
    | t  when t = Tag.v_ttcf  -> Error(`Unsupported_TTC)
    | t                       -> Error(`Unknown_flavour(t))

let d_structure d =                   (* offset table and table directory. *)
  d_version      d >>= fun () ->                          (* offset table. *)
  d_uint16       d >>= fun count ->                          (* numTables. *)
  d_skip (3 * 2) d >>= fun () ->
  set_ctx d `Table_directory;                          (* table directory. *)
  d_table_records d count

let init_decoder d =
  match d.state with
  | `Ready    -> begin d.ctx <- `Table_directory; return () end
  | `Fatal(e) -> err e
  | `Start    ->
      match d_structure d with
      | Ok() as ok -> ok
      | Error(e)   -> err_fatal d e

let flavour d =
    init_decoder d >>= fun () -> return (d.flavour)

let table_list d =
  let tags d = List.rev_map (fun (t, _, _) -> t) d.tables in
    init_decoder d >>= fun () -> return (tags d)

let table_mem d tag =
  let exists_tag tag d = List.exists (fun (t, _, _) -> tag = t) d.tables in
    init_decoder d >>= fun () -> return (exists_tag tag d)

let table_raw d tag =
  init_decoder   d >>=
  seek_table tag d >>= function
    | None      -> return None
    | Some(len) -> d_bytes len d >>= fun bytes -> return (Some(bytes))

(* convenience *)

let glyph_count d =
  init_decoder d >>=
  seek_required_table Tag.maxp d >>= fun () ->
  d_skip 4 d >>= fun () ->
  d_uint16 d >>= fun count ->
  return count

let postscript_name d = (* rigorous postscript name lookup, see OT spec p. 39 *)
  init_decoder d >>=
  seek_required_table Tag.name d >>= fun () ->
  d_uint16 d >>= fun version ->
  if version > 1 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun ncount ->
  d_uint16 d >>= fun soff ->
  let rec loop ncount () =
    if ncount = 0 then return None else
    let ncount' = ncount - 1 in
    let look_for the_eid the_lid decode =
      d_uint16 d >>= fun eid ->
      if eid <> the_eid then d_skip (4 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun lid ->
      if lid <> the_lid then d_skip (3 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun nid ->
      if nid <> 6 then d_skip (2 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun len ->
      d_uint16 d >>= fun off ->
      seek_table_pos (soff + off) d >>= fun () ->
      decode len d >>= fun name ->
      let invalid name = err (`Invalid_postscript_name(name)) in
      let name_len = String.length name in
      if name_len > 63 then invalid name else
      try
        for i = 0 to name_len - 1 do
          match Char.code name.[i] with
          | d when d < 33 || d > 126                          -> raise Exit
          | 91 | 93 | 40 | 41 | 123 | 125 | 60 | 62 | 47 | 37 -> raise Exit
          | _                                                 -> ()
        done;
        return (Some(name))
      with Exit -> invalid name
    in
    d_uint16 d >>= function
      | 3 -> look_for 1 0x409 d_utf_16be
      | 1 -> look_for 0 0 d_bytes
      | _ -> d_skip (5 * 2) d >>= loop (ncount - 1)
  in
  loop ncount ()

(* cmap table *)

type glyph_id = int
type map_kind = [ `Glyph | `Glyph_range ]

let rec d_array el count i a d =
  if i = count then return a else
  el d >>= fun v ->
  a.(i) <- v;
  d_array el count (i + 1) a d

let d_cmap_4_ranges cmap d f acc u0s u1s delta offset count =       (* ugly. *)
  let garray_pos = cur_pos d in
  let rec loop acc i =
    if i = count then return (cmap, acc) else
    let i' = i + 1 in
    let offset = offset.(i) in
    let delta = delta.(i) in
    let u0 = u0s.(i) in if not (is_cp u0) then err (`Invalid_cp u0) else
    let u1 = u1s.(i) in if not (is_cp u1) then err (`Invalid_cp u1) else
    if u0 > u1 then err (`Invalid_cp_range (u0, u1)) else
    if offset = 0 then begin
      (* The arithmetic must be performed mod 65536, this is problematic
         for Otfm's interface semantics. We need to split the range
         if the glyph range spans the bounds. *)
      let g0 = u0 + delta in
      let g1 = u1 + delta in
      if g0 < 0 && g1 >= 0 then
        let acc' = f acc `Glyph_range (u0, - delta - 1) (g0 land 65535) in
        loop (f acc' `Glyph_range (- delta, u1) 0) i'
      else
      if g0 <= 65535 && g1 > 65535 then
        let acc' = f acc `Glyph_range (u0, 65535 - delta) g0 in
        loop (f acc' `Glyph_range (65536 - delta, u1) 0) i'
      else (* glyph range is inside [0;65535] or completly outside *)
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

let d_cmap_4 cmap d f acc () =
  d_skip (3 * 2) d >>= fun () ->
  d_uint16       d >>= fun count2 ->
  let count = count2 / 2 in
  let a () = Array.make count 0 in
  d_skip (3 * 2)                  d >>= fun () ->
  d_array d_uint16 count 0 (a ()) d >>= fun u1s ->
  d_skip 2                        d >>= fun () -> (* pad *)
  d_array d_uint16 count 0 (a ()) d >>= fun u0s ->
  d_array d_int16  count 0 (a ()) d >>= fun delta ->
  d_array d_uint16 count 0 (a ()) d >>= fun offset ->
  d_cmap_4_ranges cmap d f acc u0s u1s delta offset count

let rec d_cmap_groups cmap d count f kind acc =
  if count = 0 then return (cmap, acc) else
  d_uint32_int d >>= fun u0 -> if not (is_cp u0) then err (`Invalid_cp u0) else
  d_uint32_int d >>= fun u1 -> if not (is_cp u1) then err (`Invalid_cp u1) else
  if u0 > u1 then err (`Invalid_cp_range (u0, u1)) else
  d_uint32_int d >>= fun gid ->
  d_cmap_groups cmap d (count - 1) f kind (f acc kind (u0, u1) gid)

let d_cmap_seg cmap kind d f acc () =
  d_skip (2 * 2 + 2 * 4) d >>= fun () ->
  d_uint32_int           d >>= fun count ->
  d_cmap_groups cmap d count f kind acc

let d_cmap_12 cmap d f acc () = d_cmap_seg cmap `Glyph_range d f acc ()
let d_cmap_13 cmap d f acc () = d_cmap_seg cmap `Glyph d f acc ()

let rec d_cmap_records d count acc =
  if count = 0 then return acc else
  d_uint16           d >>= fun pid ->
  d_uint16           d >>= fun eid ->
  d_uint32_int       d >>= fun pos ->
  let cur = cur_pos d in
  seek_table_pos pos d >>= fun () ->
  d_uint16           d >>= fun fmt ->
  seek_pos cur       d >>= fun () ->
  d_cmap_records d (count - 1) ((pos, pid, eid, fmt) :: acc)

let select_cmap cmaps =
  let rec loop f sel = function
  | (_, _, _, (4 | 12 | 13 as f') as c) :: cs when f' > f -> loop f (Some c) cs
  | [] -> sel
  | _ :: cs -> loop f sel cs
  in
  loop min_int None cmaps

let cmap d f acc =
  init_decoder d >>=
  seek_required_table Tag.cmap d >>= fun () ->
  d_uint16 d >>= fun version ->                           (* cmap header. *)
  if version <> 0 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun count ->                               (* numTables. *)
  d_cmap_records d count [] >>= fun cmaps ->
  match select_cmap cmaps with
  | None ->
      let drop_pos (_, pid, eid, fmt) = (pid, eid, fmt) in
      err (`Unsupported_cmaps (List.map drop_pos cmaps))
  | Some (pos, pid, eid, fmt) ->
      let cmap = match fmt with
      | 4 -> d_cmap_4 | 12 -> d_cmap_12 | 13 -> d_cmap_13 | _ -> assert false
      in
      seek_table_pos pos d >>= cmap (pid, eid, fmt) d f acc

(* glyf table *)

type glyf_loc = int

type glyph_simple_descr = (bool * int * int) list list

type glyph_composite_descr =
  (glyph_id * (int * int) * (float * float * float * float) option) list

type glyph_descr =
  [ `Simple of glyph_simple_descr
  | `Composite of glyph_composite_descr ] * (int * int * int * int)

let init_glyf d () =
  if d.glyf_pos <> -1 then return () else
  seek_required_table Tag.glyf d () >>= fun () -> d.glyf_pos <- d.i_pos; return ()

let d_rev_end_points d ccount =
  let rec loop i acc =
    if i <= 0 then return acc else
    d_uint16 d >>= fun e -> loop (i - 1) (e :: acc)
  in
  loop ccount []

let d_rev_flags d pt_count =
  let rec loop i acc =
    if i <= 0 then return acc else
    d_uint8 d >>= fun f ->
    if f land 8 = 0 then loop (i - 1) (f :: acc) else
    d_uint8 d >>= fun n ->
    let rec push n acc = if n = 0 then acc else push (n - 1) (f :: acc) in
    loop (i - 1 - n) (push (n + 1) acc)
  in
  loop pt_count []

let d_rev_coord short_mask same_mask d flags =
  let rec loop x acc = function
  | f :: fs ->
      if f land short_mask > 0 then begin
        d_uint8 d >>= fun dx ->
        let x = x + (if f land same_mask > 0 then dx else -dx) in
        loop x (x :: acc) fs
      end else begin
        if f land same_mask > 0 then loop x (x :: acc) fs else
        d_int16 d >>= fun dx ->
        let x = x + dx in
        loop x (x :: acc) fs
      end
  | [] -> return acc
  in
  loop 0 [] flags

let d_rev_xs d flags = d_rev_coord 2 16 d flags
let d_rev_ys d flags = d_rev_coord 4 32 d flags

let d_simple_glyph d ccount =
  if ccount = 0 then return [] else
  d_rev_end_points d ccount
  >>= fun rev_epts ->
  let pt_count = match rev_epts with [] -> 0 | e :: _ -> e + 1 in
  d_uint16 d
  >>= fun ins_len -> d_skip ins_len d
  >>= fun () -> d_rev_flags d pt_count
  >>= fun rev_flags ->
  let flags = List.rev rev_flags in
  d_rev_xs d flags
  >>= fun rxs -> d_rev_ys d flags
  >>= fun rys ->
  let rec combine repts flags rxs rys i acc =
    match flags with
    | []      -> acc
    | f :: fs ->
        let (new_contour, repts) =
          match repts with
          | []                 -> (false, [])
          | e :: es when e = i -> (true, es)
          | es                 -> (false, es)
        in
        match acc with
        | c :: cs ->
            let new_pt = (f land 1 > 0,  List.hd rxs, List.hd rys) in
            let acc' =
              if new_contour then [new_pt] :: c :: cs else
              (new_pt :: c) :: cs
            in
            combine repts fs (List.tl rxs) (List.tl rys) (i - 1) acc'
        | _ -> assert false
  in
  return (combine (List.tl rev_epts) rev_flags rxs rys (pt_count - 1) ([] :: []))

let d_composite_glyph d =
  let rec loop acc =
    d_uint16 d
    >>= fun flags -> d_uint16 d
    >>= fun gid ->
    if flags land 2 = 0 then err `Unsupported_glyf_matching_points else
    let dec = if flags land 1 > 0 then d_int16 else d_int8 in
    dec d
    >>= fun dx -> dec d
    >>= fun dy ->
    begin
      if flags land 8 > 0 (* scale *) then
        d_f2dot14 d >>= fun s -> return (Some (s, 0., 0., s))
      else if flags land 64 > 0 then (* xy scale *)
        d_f2dot14 d >>= fun sx ->
        d_f2dot14 d >>= fun sy -> return (Some (sx, 0., 0., sy))
      else if flags land 128 > 0 then (* m2 *)
        d_f2dot14 d >>= fun a -> d_f2dot14 d >>= fun b ->
        d_f2dot14 d >>= fun c -> d_f2dot14 d >>= fun d -> return (Some (a,b,c,d))
      else
      return None
    end
    >>= fun m ->
    let acc' = (gid, (dx, dy), m) :: acc in
    if flags land 32 > 0 then loop acc' else return (List.rev acc')
  in
  loop []

let glyf d loc =
  init_decoder d
  >>= init_glyf d
  >>= fun () -> seek_pos (d.glyf_pos + loc) d
  >>= fun () -> d_int16 d
  >>= fun ccount -> d_int16 d
  >>= fun xmin -> d_int16 d
  >>= fun ymin -> d_int16 d
  >>= fun xmax -> d_int16 d
  >>= fun ymax ->
  if ccount < -1 then err_composite_format d ccount else
  if ccount = -1
  then
    d_composite_glyph d >>= fun components ->
    return (`Composite components, (xmin, ymin, xmax, ymax))
  else
    d_simple_glyph d ccount >>= fun contours ->
    return (`Simple contours, (xmin, ymin, xmax, ymax))

(* head table *)

type head = {
  head_font_revision : int32;
  head_flags : int;
  head_units_per_em : int;
  head_created : float;
  head_modified : float;
  head_xmin : int;
  head_ymin : int;
  head_xmax : int;
  head_ymax : int;
  head_mac_style : int;
  head_lowest_rec_ppem : int;
  head_index_to_loc_format : int;
}

let head d =
  init_decoder d >>=
  seek_required_table Tag.head d >>= fun () ->
  d_uint32 d >>= fun version ->
  if version <> 0x00010000l then err_version d version else
  d_uint32 d >>= fun head_font_revision ->
  d_skip 8 d >>= fun () -> (* checkSumAdjustement, magicNumber *)
  d_uint16 d >>= fun head_flags ->
  d_uint16 d >>= fun head_units_per_em ->
  d_time   d >>= fun head_created ->
  d_time   d >>= fun head_modified ->
  d_int16  d >>= fun head_xmin ->
  d_int16  d >>= fun head_ymin ->
  d_int16  d >>= fun head_xmax ->
  d_int16  d >>= fun head_ymax ->
  d_uint16 d >>= fun head_mac_style ->
  d_uint16 d >>= fun head_lowest_rec_ppem ->
  d_skip 2 d >>= fun () -> (* fontDirectionHint *)
  d_uint16 d >>= fun head_index_to_loc_format ->
  return {
    head_font_revision; head_flags; head_units_per_em; head_created;
    head_modified; head_xmin; head_ymin; head_xmax; head_ymax;
    head_mac_style; head_lowest_rec_ppem; head_index_to_loc_format;
  }

(* hhea table *)

type hhea = {
  hhea_ascender : int;
  hhea_descender : int;
  hhea_line_gap : int;
  hhea_advance_width_max : int;
  hhea_min_left_side_bearing : int;
  hhea_min_right_side_bearing : int;
  hhea_xmax_extent : int;
  hhea_caret_slope_rise : int;
  hhea_caret_slope_run : int;
  hhea_caret_offset : int;
}

let hhea d =
  init_decoder d >>=
  seek_required_table Tag.hhea d >>= fun () ->
  d_uint32 d >>= fun version ->
  if version <> 0x00010000l then err_version d version else
  d_int16  d >>= fun hhea_ascender ->
  d_int16  d >>= fun hhea_descender ->
  d_int16  d >>= fun hhea_line_gap ->
  d_uint16 d >>= fun hhea_advance_width_max ->
  d_int16  d >>= fun hhea_min_left_side_bearing ->
  d_int16  d >>= fun hhea_min_right_side_bearing ->
  d_int16  d >>= fun hhea_xmax_extent ->
  d_int16  d >>= fun hhea_caret_slope_rise ->
  d_int16  d >>= fun hhea_caret_slope_run ->
  d_int16  d >>= fun hhea_caret_offset ->
  return {
    hhea_ascender; hhea_descender; hhea_line_gap; hhea_advance_width_max;
    hhea_min_left_side_bearing; hhea_min_right_side_bearing;
    hhea_xmax_extent; hhea_caret_slope_rise; hhea_caret_slope_run;
    hhea_caret_offset;
  }

(* hmtx table *)

let d_hm_count d =
  seek_required_table Tag.hhea d () >>= fun () ->
  d_skip (4 + 15 * 2) d >>= fun () ->
  d_uint16            d >>= fun hm_count ->
  return hm_count

let rec d_hmetric goffset i f acc last_adv d =
  if i = 0 then return (acc, last_adv) else
  d_uint16 d >>= fun adv ->
  d_int16  d >>= fun lsb ->
  let acc' = f acc (goffset - i) adv lsb in
  d_hmetric goffset (i - 1) f acc' adv d

let rec d_hlsb goffset i f acc adv d =
  if i = 0 then return acc else
  d_int16 d >>= fun lsb ->
  let acc' = f acc (goffset - i) adv lsb in
  d_hlsb goffset (i - 1) f acc' adv d

let hmtx d f acc =
  glyph_count d >>= fun glyph_count ->
  d_hm_count  d >>= fun hm_count ->
  seek_required_table Tag.hmtx d () >>= fun () ->
  d_hmetric hm_count hm_count f acc (-1) d >>= fun (acc, last_adv) ->
  d_hlsb glyph_count (glyph_count - hm_count) f acc last_adv d

(* name table *)

(* Source: https://skia.googlecode.com/svn/trunk/src/sfnt/SkOTTable_name.cpp
   BSD3 licensed (c) 2011 Google Inc. *)
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

let rec d_name_langs soff ncount d =
  d_skip (ncount * 6 * 2) d >>= fun () ->
  d_uint16                d >>= fun lcount ->
  let rec loop i acc =
    if ncount = 0 then return acc else
    d_uint16 d >>= fun len ->
    d_uint16 d >>= fun off ->
    let cpos = cur_pos d in
    seek_table_pos (soff + off) d >>= fun () ->
    d_utf_16be len d >>= fun lang ->
    seek_pos cpos d >>= fun () ->
    loop (i - 1) ((0x8000 + (ncount - i), lang) :: acc)
  in
  loop ncount []

let rec d_name_records soff ncount f acc langs seen d =
  if ncount = 0 then return acc else
  d_uint16 d >>= fun pid ->
  d_uint16 d >>= fun eid ->
  d_uint16 d >>= fun lid ->
  d_uint16 d >>= fun nid ->
  d_uint16 d >>= fun len ->
  d_uint16 d >>= fun off ->
  match pid, eid with
  | (0 | 2), _ | 3, 1 ->
      let cpos = cur_pos d in
      let n = (nid, lid) in
      if List.mem n seen
      then d_name_records soff (ncount - 1) f acc langs seen d
      else
      seek_table_pos (soff + off) d >>= fun () ->
      d_utf_16be len d >>= fun v ->
      seek_pos cpos  d >>= fun () ->
      let lang = try List.assoc lid langs with Not_found -> "und" in
      let acc' = f acc nid lang v in
      d_name_records soff (ncount - 1) f acc' langs (n :: seen) d
  | _ ->
      d_name_records soff (ncount - 1) f acc langs seen d

let name d f acc =
  init_decoder d >>=
  seek_required_table Tag.name d >>= fun () ->
  d_uint16 d >>= fun version ->
  if version < 0 || version > 1 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun ncount ->
  d_uint16 d >>= fun soff ->
  let cpos = cur_pos d in
  (if version = 0 then return [] else d_name_langs soff ncount d) >>= fun langs ->
  let langs = List.rev_append langs lcid_to_bcp47 in
  seek_pos cpos d >>= fun () ->
  d_name_records soff ncount f acc langs [] d

(* OS/2 table *)

type os2 =
  { os2_x_avg_char_width : int;
    os2_us_weight_class : int;
    os2_us_width_class : int;
    os2_fs_type : int;
    os2_y_subscript_x_size : int;
    os2_y_subscript_y_size : int;
    os2_y_subscript_x_offset : int;
    os2_y_subscript_y_offset : int;
    os2_y_superscript_x_size : int;
    os2_y_superscript_y_size : int;
    os2_y_superscript_x_offset : int;
    os2_y_superscript_y_offset : int;
    os2_y_strikeout_size : int;
    os2_y_strikeout_position : int;
    os2_family_class : int;
    os2_panose : string; (* 10 bytes *)
    os2_ul_unicode_range1 : int32;
    os2_ul_unicode_range2 : int32;
    os2_ul_unicode_range3 : int32;
    os2_ul_unicode_range4 : int32;
    os2_ach_vend_id : int32;
    os2_fs_selection : int;
    os2_us_first_char_index : int;
    os2_us_last_char_index : int;
    os2_s_typo_ascender : int;
    os2_s_type_descender : int;
    os2_s_typo_linegap : int;
    os2_us_win_ascent : int;
    os2_us_win_descent : int;
    os2_ul_code_page_range_1 : int32 option;
    os2_ul_code_page_range_2 : int32 option;
    os2_s_x_height : int option;
    os2_s_cap_height : int option;
    os2_us_default_char : int option;
    os2_us_break_char : int option;
    os2_us_max_context : int option; }

let os2 d =
  init_decoder d >>=
  seek_required_table Tag.os2 d >>= fun () ->
  d_uint16 d >>= fun version ->
  if version > 0x0004 then err_version d (Int32.of_int version) else
  let opt v dec d =
    if version < v then return None else dec d >>= fun v -> return (Some(v))
  in
  d_int16  d >>= fun os2_x_avg_char_width ->
  d_uint16 d >>= fun os2_us_weight_class ->
  d_uint16 d >>= fun os2_us_width_class ->
  d_uint16 d >>= fun os2_fs_type ->
  d_int16  d >>= fun os2_y_subscript_x_size ->
  d_int16  d >>= fun os2_y_subscript_y_size ->
  d_int16  d >>= fun os2_y_subscript_x_offset ->
  d_int16  d >>= fun os2_y_subscript_y_offset ->
  d_int16  d >>= fun os2_y_superscript_x_size ->
  d_int16  d >>= fun os2_y_superscript_y_size ->
  d_int16  d >>= fun os2_y_superscript_x_offset ->
  d_int16  d >>= fun os2_y_superscript_y_offset ->
  d_int16  d >>= fun os2_y_strikeout_size ->
  d_int16  d >>= fun os2_y_strikeout_position ->
  d_int16  d >>= fun os2_family_class ->
  d_bytes 10 d >>= fun os2_panose ->
  d_uint32 d >>= fun os2_ul_unicode_range1 ->
  d_uint32 d >>= fun os2_ul_unicode_range2 ->
  d_uint32 d >>= fun os2_ul_unicode_range3 ->
  d_uint32 d >>= fun os2_ul_unicode_range4 ->
  d_uint32 d >>= fun os2_ach_vend_id ->
  d_uint16 d >>= fun os2_fs_selection ->
  d_uint16 d >>= fun os2_us_first_char_index ->
  d_uint16 d >>= fun os2_us_last_char_index ->
  d_int16  d >>= fun os2_s_typo_ascender ->
  d_int16  d >>= fun os2_s_type_descender ->
  d_int16  d >>= fun os2_s_typo_linegap ->
  d_uint16 d >>= fun os2_us_win_ascent ->
  d_uint16 d >>= fun os2_us_win_descent ->
  opt 0x0001 d_uint32 d >>= fun os2_ul_code_page_range_1 ->
  opt 0x0001 d_uint32 d >>= fun os2_ul_code_page_range_2 ->
  opt 0x0002 d_int16  d >>= fun os2_s_x_height ->
  opt 0x0002 d_int16  d >>= fun os2_s_cap_height ->
  opt 0x0002 d_uint16 d >>= fun os2_us_default_char ->
  opt 0x0002 d_uint16 d >>= fun os2_us_break_char ->
  opt 0x0002 d_uint16 d >>= fun os2_us_max_context ->
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

(* kern table *)

type kern_info ={
  kern_dir : [ `H | `V ];
  kern_kind : [ `Min | `Kern ];
  kern_cross_stream : bool;
}

let kern_info c =
  {
    kern_dir = (if c land 0x1 > 0 then `H else `V);
    kern_kind = (if c land 0x2 > 0 then `Min else `Kern);
    kern_cross_stream = c land 0x4 > 0;
  }

let rec kern_tables ntables t p acc d =
  if ntables = 0 then return acc else
  d_uint16 d >>= fun version ->
  if version > 0 then err_version d (Int32.of_int version) else
  d_uint16 d >>= fun len ->
  d_uint16 d >>= fun coverage ->
  let format = coverage lsr 8 in
  let skip acc =
    d_skip (len - 3 * 2) d >>= fun () ->
    kern_tables (ntables - 1) t p acc d
  in
  if format <> 0 then skip acc else
  match t acc (kern_info coverage) with
  | `Skip, acc -> skip acc
  | `Fold, acc ->
      let rec d_pairs len acc d =
        if len < 3 * 2 then d_skip len d >>= fun () -> return acc else
        d_uint16 d >>= fun left ->
        d_uint16 d >>= fun right ->
        d_int16 d >>= fun values ->
        d_pairs (len - 3 * 2) (p acc left right values) d
      in
      d_skip (4 * 2)  d >>= fun () ->
      d_pairs len acc d >>= fun acc ->
      kern_tables (ntables - 1) t p acc d

let kern d t p acc =
  init_decoder d >>=
  seek_table Tag.kern d >>= function
  | None    -> return acc
  | Some(_) ->
      d_uint16 d >>= fun version ->
      if version > 0 then err_version d (Int32.of_int version) else
      d_uint16 d >>= fun ntables ->
      kern_tables ntables t p acc d

(* loca table *)

let d_loca_format d () =
  d_uint16 d >>= fun f -> if f > 1 then err_loca_format d f else return f

let init_loca d () =
  if d.loca_pos <> -1 then return () else
  seek_required_table Tag.head d ()
  >>= fun () -> d_skip 50 d
  >>= d_loca_format d
  >>= fun loca_format ->
  d.loca_format <- loca_format;
  seek_required_table Tag.loca d ()
  >>= fun () -> d.loca_pos <- d.i_pos;
  return ()

let loca_short d gid =
  seek_pos (d.loca_pos + gid * 2) d
  >>= fun () -> d_uint16 d
  >>= fun o1 -> d_uint16 d
  >>= fun o2 ->
  let o1 = o1 * 2 in
  let o2 = o2 * 2 in
  if o1 = o2 then return None else return (Some o1)

let loca_long d gid =
  seek_pos (d.loca_pos + gid * 4) d
  >>= fun () -> d_uint32_int d
  >>= fun o1 -> d_uint32_int d
  >>= fun o2 -> if o1 = o2 then return None else return (Some o1)

let loca d gid =
  init_decoder d
  >>= init_loca d
  >>= fun () -> if d.loca_format = 0 then loca_short d gid else loca_long d gid

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

let print_for_debug msg = () (* print_endline msg *)

let print_for_debug_int name v = print_for_debug (name ^ " = " ^ (string_of_int v))


type 'a ok = ('a, error) result

type gsub_subtable =
  | LigatureSubtable of (glyph_id * (glyph_id list * glyph_id) list) list
  (* temporary; should contain more lookup type *)


let confirm b e =
  if not b then err e else return ()


let seek_pos_from_list origin scriptTag d =
  let rec aux i =
    if i <= 0 then return false else
    begin
      d_bytes 4 d >>= fun tag ->
      d_uint16 d  >>= fun offset ->
(*        print_for_debug ("| tag = '" ^ tag ^ "'");  (* for debug *) *)
        if String.equal tag scriptTag then
          seek_pos (origin + offset) d >>= fun () ->
          return true
        else
          aux (i - 1)
    end
  in
  d_uint16 d >>= fun scriptCount ->
  print_for_debug_int "scriptCount" scriptCount;
  aux scriptCount >>= fun found ->
  return found


let d_list_filtered df indexlst d =
  let rec aux acc imax i =
    if i >= imax then return (List.rev acc) else
    df d >>= fun data ->
    if List.mem i indexlst then
      aux (data :: acc) imax (i + 1)
    else
      aux acc imax (i + 1)
  in
    d_uint16 d >>= fun count ->
    print_for_debug_int "count" count;
    aux [] count 0


let d_repeat n df d =
  let rec aux acc i =
    if i <= 0 then return (List.rev acc) else
    df d >>= fun data ->
    aux (data :: acc) (i - 1)
  in
  aux [] n


let d_list df d =
  d_uint16 d >>= fun count ->
  print_for_debug_int "(d_list) count" count;
  d_repeat count df d


let d_offset_list (offset_origin : int) d : (int list) ok =
  d_list d_uint16 d >>= fun reloffsetlst ->
  return (reloffsetlst |> List.map (fun reloffset -> offset_origin + reloffset))


let d_offset (offset_origin : int) d : int ok =
  d_uint16 d >>= fun reloffset ->
  return (offset_origin + reloffset)

let d_range_record d =
  let rec range acc i j =
    if i > j then List.rev acc else
      range (i :: acc) (i + 1) j
  in
  d_uint16 d >>= fun start_gid ->
  d_uint16 d >>= fun end_gid ->
  d_uint16 d >>= fun _ -> (* -- startCoverageIndex; can be ignored -- *)
  return (range [] start_gid end_gid)


let d_coverage_main d : (glyph_id list) ok =
    (* -- the position is supposed to be set
          to the beginning of a Coverage table [page 139] -- *)
  d_uint16 d >>= fun coverageFormat ->
  print_for_debug_int "coverageFormat" coverageFormat;  (* for debug *)
  let res =  (* for debug *)
    match coverageFormat with
    | 1 -> d_list d_uint16 d
    | 2 -> d_list d_range_record d >>= fun rnglst -> return (List.concat rnglst)
    | _ -> err_version d (Int32.of_int coverageFormat)
  in print_for_debug "end Coverage table"; res  (* for debug *)


let d_coverage offset_origin d : (glyph_id list) ok =
    (* -- the position is supposed to be set
          just before an offset field to a coverage table -- *)
  let pos_first = cur_pos d in
  d_offset offset_origin d   >>= fun offset_Coverage ->
  seek_pos offset_Coverage d >>= fun () ->
  d_coverage_main d          >>= fun gidlst ->
  seek_pos (pos_first + 2) d >>= fun () ->
  return gidlst


let seek_every_pos (type a) (offsetlst : int list) (df : decoder -> a ok) (d : decoder) : (a list) ok =
  let rec aux acc offsetlst =
  match offsetlst with
  | []             -> return (List.rev acc)
  | offset :: tail ->
(*      let () = print_for_debug ("| offset = " ^ (string_of_int offset)) in  (* for debug *) *)
      seek_pos offset d >>= fun () ->
      df d >>= fun data ->
      aux (data :: acc) tail
  in
    aux [] offsetlst


let d_with_coverage (type a) (offset_Substitution_table : int) (df : decoder -> a ok) d : ((glyph_id * a) list) ok =
    (* -- the position is supposed to be set
          just before a Coverage field and a subsequent offset list
          [page 254 etc.] -- *)
  d_coverage offset_Substitution_table d >>= fun coverage ->
  print_for_debug_int "size of Coverage" (List.length coverage);  (* for debug *)
  List.iter (print_for_debug_int "  *   elem") coverage;  (* for debug *)

    (* -- the position is set just before LigSetCount field [page 254] -- *)
  d_offset_list offset_Substitution_table d >>= fun offsetlst_LigatureSet ->
  print_for_debug_int "number of LigatureSet" (List.length offsetlst_LigatureSet);  (* for debug *)
  seek_every_pos offsetlst_LigatureSet df d >>= fun datalst ->
  try
    return (List.combine coverage datalst)
  with
  | Invalid_argument(_) -> err (`Inconsistent_length_of_coverage(`Table(Tag.gsub)))


let advanced_table_scheme tag_Gxxx lookup d scriptTag langSysTag_opt featureTag : 'a ok =
  init_decoder d >>=
  seek_table tag_Gxxx d >>= function
    | None    -> Error(`Missing_required_table(tag_Gxxx))
    | Some(_) ->
        let offset_Gxxx = cur_pos d in
        d_uint32 d >>= fun version ->
        confirm (version = 0x00010000l) (e_version d version) >>= fun () ->
        d_offset offset_Gxxx d >>= fun offset_ScriptList ->
        d_offset offset_Gxxx d >>= fun offset_FeatureList ->
        d_offset offset_Gxxx d >>= fun offset_LookupList ->
        print_for_debug_int "offset_ScriptList" offset_ScriptList;  (* for debug *)
        seek_pos offset_ScriptList d >>= fun () ->
        seek_pos_from_list offset_ScriptList scriptTag d >>= fun found ->
        confirm found (`Missing_required_script_tag(scriptTag)) >>= fun () ->
          (* -- now the position is set to the beginning of the required Script table -- *)
        let offset_Script_table = cur_pos d in
        print_for_debug_int "offset_Script_table" offset_Script_table;  (* for debug *)
        d_offset offset_Script_table d >>= fun offset_DefaultLangSys ->
        begin
          match langSysTag_opt with
          | None -> seek_pos offset_DefaultLangSys d
          | Some(langSysTag) ->
              begin
                seek_pos_from_list offset_Script_table langSysTag d >>= fun found ->
                confirm found (`Missing_required_langsys_tag(langSysTag))
              end
        end >>= fun () ->
          (* -- now the position is set to the beginning of the required LangSys table *)
        d_uint16 d >>= fun offset_LookupOrder ->
        confirm (offset_LookupOrder = 0) (`Invalid_lookup_order(offset_LookupOrder)) >>= fun () ->
        d_uint16 d >>= fun reqFeatureIndex ->
        let () =  (* for debug *)
          if reqFeatureIndex = 0xFFFF then
            print_for_debug "no feature is specified as default."  (* for debug *)
          else
            ()
        in
        d_list d_uint16 d >>= fun featrindexlst ->
          (* -- now we are going to see FeatureList table -- *)
        print_for_debug_int "offset_FeatureList" offset_FeatureList;  (* for debug *)
        seek_pos offset_FeatureList d >>= fun () ->
        print_for_debug "---- FeatureList table ----";  (* for debug *)
        d_list_filtered (fun d ->
          d_bytes 4 d >>= fun tag ->
(*
          print_for_debug ("| tag = '" ^ tag ^ "'");  (* for debug *)
*)
          d_offset offset_FeatureList d >>= fun offset -> return (tag, offset))
          featrindexlst d >>= fun pairlst ->
        begin
          try return (List.assoc featureTag pairlst) with
          | Not_found -> err (`Missing_required_feature_tag(featureTag))
        end >>= fun offset_Feature_table ->
        print_for_debug_int "offset_Feature_table" offset_Feature_table;  (* for debug *)
        seek_pos offset_Feature_table d >>= fun () ->
          (* -- now the position is set to the beginning of the required Feature table -- *)
        print_for_debug "---- Feature table ----";  (* for debug *)
        d_offset offset_Feature_table d >>= fun offset_FeatureParams ->
        d_list d_uint16 d >>= fun lookuplst ->
          (* -- now we are going to see LookupList table -- *)
        print_for_debug_int "offset_LookupList" offset_LookupList;  (* for debug *)
        seek_pos offset_LookupList d >>= fun () ->
        d_list_filtered (d_offset offset_LookupList) lookuplst d >>= fun offsetlst ->
        seek_every_pos offsetlst lookup d


let d_ligature_table d : (glyph_id list * glyph_id) ok =
    (* -- the position is supposed to be set
          to the beginning of a Ligature table [page 255] -- *)
  d_uint16 d >>= fun ligGlyph ->
  d_uint16 d >>= fun compCount ->
  print_for_debug (Printf.sprintf "    [ligGlyph = %d ----> compCount = %d]" ligGlyph compCount);
  d_repeat (compCount - 1) d_uint16 d >>= fun component ->
  return (component, ligGlyph)


let d_ligature_set_table d : ((glyph_id list * glyph_id) list) ok =
    (* -- the position is supposed to be set
          to the beginning of a LigatureSet table [page 254] -- *)
  let offset_LigatureSet_table = cur_pos d in
  print_for_debug_int "offset_LigatureSet_table" offset_LigatureSet_table;
  d_offset_list offset_LigatureSet_table d >>= fun offsetlst_Ligature_table ->
  seek_every_pos offsetlst_Ligature_table d_ligature_table d


let d_ligature_substitution_subtable d : ((glyph_id * (glyph_id list * glyph_id) list) list) ok =
    (* -- the position is supposed to be set
          to the beginning of Ligature SubstFormat1 subtable [page 254] -- *)
  let offset_Substitution_table = cur_pos d in
  print_for_debug_int "offset_Substitution_table" offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  print_for_debug_int "substFormat" substFormat;  (* for debug *)
  confirm (substFormat = 1) (e_version d (Int32.of_int substFormat)) >>= fun () ->
  d_with_coverage offset_Substitution_table d_ligature_set_table d


let lookup_gsub d : gsub_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of a Lookup table [page 137] -- *)
  let offset_Lookup_table = cur_pos d in
  d_uint16 d >>= fun lookupType ->
  print_for_debug ("# lookupType = " ^ (string_of_int lookupType));  (* for debug *)
  d_uint16 d >>= fun lookupFlag ->
  print_for_debug ("# lookupFlag = " ^ (string_of_int lookupFlag));  (* for debug *)
(*
  let rightToLeft      = 0 < lookupFlag land 1 in
  let ignoreBaseGlyphs = 0 < lookupFlag land 2 in
  let ignoreLigatures  = 0 < lookupFlag land 4 in
  let ignoreMarks      = 0 < lookupFlag land 8 in
*)
  match lookupType with
  | 1  (* -- single substitution -- *) ->
      failwith "single substitution; remains to be supported."  (* temporary *)
  | 2  (* -- multiple substitution -- *) ->
      failwith "multiple substitution; remains to be supported."  (* temporary *)
  | 3  (* -- alternate substitution -- *) ->
      failwith "alternate substitution; remains to be supported."  (* temporary *)
  | 4  (* -- ligature substitution -- *) ->
      let () = print_for_debug "lookupType 4" in  (* for debug *)
      d_offset_list offset_Lookup_table d >>= fun offsetlst_SubTable ->
      let () = print_for_debug_int "number of subtables" (List.length offsetlst_SubTable) in  (* for debug *)
      seek_every_pos offsetlst_SubTable d_ligature_substitution_subtable d >>= fun gidfst_ligset_assoc ->
      return (LigatureSubtable(List.concat gidfst_ligset_assoc))
  | _ ->
      failwith "lookupType >= 5; remains to be supported (or font file broken)."  (* temporary *)


let rec fold_subtables_gsub (f_lig : 'a -> glyph_id * (glyph_id list * glyph_id) list -> 'a) (init : 'a) (subtablelst : gsub_subtable list) : 'a =
  let iter = fold_subtables_gsub f_lig in
    match subtablelst with
    | [] -> init

    | LigatureSubtable(gidfst_ligset_assoc) :: tail ->
        let initnew = List.fold_left f_lig init gidfst_ligset_assoc in
          iter initnew tail


let gsub d scriptTag langSysTag_opt featureTag f_lig init =
  advanced_table_scheme Tag.gsub lookup_gsub d scriptTag langSysTag_opt featureTag
    >>= fun subtables -> return (fold_subtables_gsub f_lig init subtables)


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

type gpos_subtable =
  | PairPosAdjustment1 of (glyph_id * (glyph_id * value_record * value_record) list) list
  | PairPosAdjustment2 of class_definition list * class_definition list * (class_value * (class_value * value_record * value_record) list) list
  | ExtensionPos      of gpos_subtable list
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
    | []           -> List.rev acc
    | head :: tail -> aux ((i, head) :: acc) (i + 1) tail
  in
    aux [] 0 lst


let d_class_1_record class2Count valfmt1 valfmt2 d : ((class_value * value_record * value_record) list) ok =
  d_repeat class2Count (d_class_2_record valfmt1 valfmt2) d >>= fun pairlst ->
  try return (numbering pairlst |> List.map (fun (x, (y, z)) -> (x, y, z))) with
  | Invalid_argument(_) -> err `Inconsistent_length_of_class


let d_class_definition_format_1 d : (class_definition list) ok =
  let rec aux acc gidstt lst =
    match lst with
    | []          -> return (List.rev acc)
    | cls :: tail -> aux (GlyphToClass(gidstt, cls) :: acc) (gidstt + 1) tail
  in
    d_uint16 d >>= fun startGlyph ->
    d_list d_class d >>= fun classValueArray ->
    aux [] startGlyph classValueArray


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
  | _ -> err_version d (Int32.of_int classFormat)


let d_fetch offset_origin df d =
  let pos_before = cur_pos d in
  d_offset offset_origin d >>= fun offset ->
  seek_pos offset d >>= fun () ->
  df d >>= fun res ->
  seek_pos (pos_before + 2) d >>= fun () ->
  return res


let d_pair_adjustment_subtable d : gpos_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of a PairPos subtable [page 194] -- *)
  let offset_PairPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  match posFormat with
  | 1 ->
      d_coverage offset_PairPos d >>= fun coverage ->
      d_value_format d >>= fun valueFormat1 ->
      d_value_format d >>= fun valueFormat2 ->
      d_list (d_offset offset_PairPos) d >>= fun offsetlst_PairSet ->
      seek_every_pos offsetlst_PairSet (d_pair_set valueFormat1 valueFormat2) d >>= fun pairsetlst ->
      begin
        try return (PairPosAdjustment1(List.combine coverage pairsetlst)) with
        | Invalid_argument(_) -> err (`Inconsistent_length_of_coverage(`Table(Tag.gsub)))
      end

  | 2 ->
      d_coverage offset_PairPos d >>= fun coverage ->
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

  | _ -> err_version d (Int32.of_int posFormat)


let lookup_gpos_exact offsetlst_SubTable lookupType d : (gpos_subtable list) ok =
  match lookupType with
  | 1  (* -- Single adjustment -- *) ->
      failwith "Single adjustment; remains to be supported."  (* temporary *)
  | 2  (* -- Pair adjustment -- *) ->
      let () = print_for_debug_int "number of subtables" (List.length offsetlst_SubTable) in  (* for debug *)
      seek_every_pos offsetlst_SubTable d_pair_adjustment_subtable d
  | 3  (* -- Cursive attachment -- *) ->
      failwith "Cursive attachment; remains to be supported."  (* temporary *)
  | 4  (* -- MarkToBase attachment -- *) ->
      failwith "MarkToBase attachment; remains to be supported."  (* temporary *)
  | 9  (* -- Extension positioning [page 213] -- *) ->
      err `Invalid_extension_position
  | _ ->
      failwith "lookupType other; remains to be supported (or font file broken)."  (* temporary *)


let d_extension_position d =
    (* -- the position is supposed to be set
          to the beginning of ExtensionPosFormat1 subtable [page 213] -- *)
  let offset_ExtensionPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  confirm (posFormat = 1) (e_version d (Int32.of_int posFormat)) >>= fun () ->
  d_uint16 d >>= fun extensionLookupType ->
  d_uint32 d >>= fun extensionOffset ->
  let offset = offset_ExtensionPos + (Int32.to_int extensionOffset) in
  lookup_gpos_exact [offset] extensionLookupType d


let lookup_gpos d : gpos_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of Lookup table [page 137] -- *)
  let offset_Lookup_table = cur_pos d in
  d_uint16 d >>= fun lookupType ->
  print_for_debug ("# lookupType = " ^ (string_of_int lookupType));  (* for debug *)
  d_uint16 d >>= fun lookupFlag ->
  print_for_debug ("# lookupFlag = " ^ (string_of_int lookupFlag));  (* for debug *)
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
      return (ExtensionPos(List.concat subtablelstlst))
  | _ ->
      lookup_gpos_exact offsetlst_SubTable lookupType d >>= fun subtablelst ->
      return (ExtensionPos(subtablelst))  (* ad-hoc fix *)


let rec fold_subtables_gpos (f_pair1 : 'a -> glyph_id * (glyph_id * value_record * value_record) list -> 'a) (f_pair2 : class_definition list -> class_definition list -> 'a -> (class_value * (class_value * value_record * value_record) list) list -> 'a) (init : 'a) (subtablelst : gpos_subtable list) : 'a =
  let iter = fold_subtables_gpos f_pair1 f_pair2 in
    match subtablelst with
    | [] -> init

    | PairPosAdjustment1(gidfst_pairposlst_assoc) :: tail ->
        let initnew = List.fold_left f_pair1 init gidfst_pairposlst_assoc in
          iter initnew tail

    | PairPosAdjustment2(clsdeflst1, clsdeflst2, cls_pairposlst_assoc) :: tail ->
        let initnew = f_pair2 clsdeflst1 clsdeflst2 init cls_pairposlst_assoc in
          iter initnew tail

    | ExtensionPos(subtablelstsub) :: tail ->
        let initnew = iter init subtablelstsub in
          iter initnew tail


let gpos d scriptTag langSysTag_opt featureTag f_pair1 f_pair2 init =
  advanced_table_scheme Tag.gpos lookup_gpos d scriptTag langSysTag_opt featureTag
    >>= fun subtables -> return (fold_subtables_gpos f_pair1 f_pair2 init subtables)


(* -- CFF_ table -- *)

type offsize = OffSize1 | OffSize2 | OffSize3 | OffSize4
type cff_key = ShortKey of int | LongKey of int
type cff_value = Integer of int | Real of float
type dict_element = Value of cff_value | Key of cff_key

module DictMap = Map.Make
  (struct
    type t = cff_key
    let compare kt1 kt2 =
      match (kt1, kt2) with
      | (ShortKey(i1), ShortKey(i2)) -> Pervasives.compare i1 i2
      | (ShortKey(_), LongKey(_))    -> -1
      | (LongKey(_), ShortKey(_))    -> 1
      | (LongKey(i1), LongKey(i2))   -> Pervasives.compare i1 i2
  end)

type cff_info = (string option * (cff_value list) DictMap.t) option

type cff_top_dict =
  {
    is_fixed_pitch : bool;
    italic_angle : int;
    underline_position : int;
    underline_thickness : int;
    paint_type : int;
    charstring_type : int;
    (* font_matrix : float * float * float * float; *)
    font_bbox : int * int * int * int;
    stroke_width : int;
  }


let (~@) = Int32.of_int
let (?@) = Int32.to_int
let (-@) = Int32.sub
let is_in_range a b x = (a <= x && x <= b)


let d_offsize d =
  d_uint8 d >>= fun i ->
    match i with
    | 1 -> return OffSize1
    | 2 -> return OffSize2
    | 3 -> return OffSize3
    | 4 -> return OffSize4
    | n -> err (`Invalid_cff_not_an_offsize(n))

let d_cff_offset ofsz d =
  match ofsz with
  | OffSize1 -> d_uint8  d >>= fun i -> return (~@ i)
  | OffSize2 -> d_uint16 d >>= fun i -> return (~@ i)
  | OffSize3 -> d_uint24 d >>= fun i -> return (~@ i)
  | OffSize4 -> d_uint32 d

let d_cff_offset_singleton ofsz dl d =
  d_cff_offset ofsz d                                            >>= fun offset1 ->
  confirm (offset1 = ~@ 1) `Invalid_cff_invalid_first_offset >>= fun () ->
  d_cff_offset ofsz d                                            >>= fun offset2 ->
  dl (offset2 -@ offset1) d

let d_index_singleton dl d =
  d_uint8 d                                        >>= fun count ->
  confirm (count = 1) `Invalid_cff_not_a_singleton >>= fun () ->
  d_offsize d                                      >>= fun offSize ->
  d_cff_offset_singleton offSize dl d                  >>= fun v ->
  return v

let d_cff_offset_list ofsz count d =
  let rec aux offsetprev acc i =
    if i >= count then return (List.rev acc) else
    d_cff_offset ofsz d >>= fun offset ->
    aux offset ((offset -@ offsetprev) :: acc) (i + 1)
  in
  d_cff_offset ofsz d                                            >>= fun offset1 ->
  confirm (offset1 = ~@ 1) `Invalid_cff_invalid_first_offset >>= fun () ->
  aux (~@ 1) [] 0

let d_index dl d =
  let rec loop_data acc = function
    | []          -> return (List.rev acc)
    | len :: tail ->
        dl len d >>= fun v ->
        loop_data (v :: acc) tail
  in
  d_uint8 d                     >>= fun count ->
  Printf.printf "count: %d\n" count;  (* for debug *)
  if count = 0 then return [] else
  d_offsize d                   >>= fun offSize ->
  d_cff_offset_list offSize count d >>= fun lenlst ->
  loop_data [] lenlst

let d_dict_element d =
  d_uint8 d >>= function
    | b0  when b0 |> is_in_range 32 246 ->
        return (1, Value(Integer(b0 - 139)))
    | b0  when b0 |> is_in_range 247 250 ->
        d_uint8 d >>= fun b1 ->
        return (2, Value(Integer((b0 - 247) * 256 + b1 + 108)))
    | b0  when b0 |> is_in_range 251 254 ->
        d_uint8 d >>= fun b1 ->
        return (2, Value(Integer(-(b0 - 251) * 256 - b1 - 108)))
    | 28 ->
        d_uint8 d >>= fun b1 ->
        d_uint8 d >>= fun b2 ->
        return (3, Value(Integer((b1 lsl 8) lor b2)))
    | 29 ->
        d_uint8 d >>= fun b1 ->
        d_uint8 d >>= fun b2 ->
        d_uint8 d >>= fun b3 ->
        d_uint8 d >>= fun b4 ->
        return (5, Value(Integer((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4)))
    | 30 ->
        failwith "a real number operand; remains to be implemented."
    | k0  when k0 |> is_in_range 0 11 ->
        return (1, Key(ShortKey(k0)))
    | k0  when k0 |> is_in_range 13 21 ->
        return (1, Key(ShortKey(k0)))
    | 12 ->
        d_uint8 d >>= fun k1 ->
        return (2, Key(LongKey(k1)))
    | _ ->
        err `Invalid_cff_not_an_element

let pp_element ppf = function
  | Value(Integer(i)) -> Format.fprintf ppf "Integer(%d)" i
  | Value(Real(r))    -> Format.fprintf ppf "Real(_)"
  | Key(LongKey(x))   -> Format.fprintf ppf "LongKey(%d)" x
  | Key(ShortKey(x))  -> Format.fprintf ppf "ShortKey(%d)" x

let d_dict_keyval d =
  let rec aux stepsum vacc =
    d_dict_element d >>= fun (step, elem) ->
      Format.eprintf "element: %d, %a\n" step pp_element elem;  (* for debug *)
      match elem with
      | Value(v) -> aux (stepsum + step) (v :: vacc)
      | Key(k)   -> return (stepsum + step, List.rev vacc, k)
  in
    aux 0 []

let d_dict len d =
  let rec loop_keyval mapacc len d =
    if len = -1 (* doubtful*) then return mapacc else
    if len < -1 (* doubtful*) then err `Invalid_cff_inconsistent_length else
      d_dict_keyval d >>= fun (step, vlst, k) ->
      Printf.printf "step: %d\n" step;  (*for debug *)
      loop_keyval (mapacc |> DictMap.add k vlst) (len - step) d
  in
    Printf.printf "length: %d\n" len;  (* for debug *)
    loop_keyval DictMap.empty len d

let cff_info d =
  init_decoder d >>=
  seek_required_table Tag.cff d >>= fun () ->
  (* Header: *)
    Printf.printf "Header\n";  (* for debug *)
    d_uint8 d              >>= fun major ->
    d_uint8 d              >>= fun minor ->
    d_uint8 d              >>= fun hdrSize ->
    d_offsize d            >>= fun offSizeGlobal ->
    d_skip (hdrSize - 4) d >>= fun () ->
  (* Name INDEX (should contain only one element): *)
    Printf.printf "Name INDEX\n";  (* for debug *)
    d_index (fun i -> d_bytes (?@ i)) d >>= fun namelst ->
    begin
      match namelst with
      | []          -> return None
      | _ :: _ :: _ -> err `Invalid_cff_not_a_singleton
      | n :: []     -> return (Some(n))
    end >>= fun name ->
  (* Top DICT INDEX (should contain only one DICT): *)
    Printf.printf "Top DICT INDEX\n";  (* for debug *)
    d_index (fun i -> d_dict (?@ i)) d  >>= function
      | []            -> err `Invalid_cff_not_a_singleton
      | _ :: _ :: _   -> err `Invalid_cff_not_a_singleton
      | dictmap :: [] ->
(*
  (* String INDEX: *)
    Printf.printf "String INDEX\n";  (* for debug *)
    d_index (fun i -> d_bytes (?@ i)) d  >>= fun stringlst ->
  (* Global Subr INDEX: *)
    Printf.printf "Global Subr INDEX\n";  (* for debug *)
    d_index (fun i -> d_bytes (?@ i)) d  >>= fun subrlst ->
*)
    return (Some((name, dictmap)))

let cff_top_dict = function
  | None               -> return None
  | Some((_, dictmap)) ->
      let get_integer key dflt =
        try
          let dictvlst = DictMap.find key dictmap in
            match dictvlst with
            | Integer(i) :: [] -> return i
            | _                -> err `Invalid_cff_not_an_integer
        with
        | Not_found -> return dflt
      in
      let get_boolean key dflt =
        get_integer key (if dflt then 1 else 0) >>= fun i -> return (i <> 0)
      in
      let get_iquad key dflt =
        try
          let dictvlst = dictmap |> DictMap.find key in
            match dictvlst with
            | Integer(i1) :: Integer(i2) :: Integer(i3) :: Integer(i4) :: [] -> return (i1, i2, i3, i4)
            | _                                                              -> err `Invalid_cff_not_a_quad
        with
        | Not_found -> return dflt
      in
        get_boolean (LongKey(1)) false       >>= fun is_fixed_pitch ->
        get_integer (LongKey(2)) 0           >>= fun italic_angle ->
        get_integer (LongKey(3)) (-100)      >>= fun underline_position ->
        get_integer (LongKey(4)) 50          >>= fun underline_thickness ->
        get_integer (LongKey(5)) 0           >>= fun paint_type ->
        get_integer (LongKey(6)) 2           >>= fun charstring_type ->
        get_iquad (ShortKey(5)) (0, 0, 0, 0) >>= fun font_bbox ->
        get_integer (LongKey(8)) 0           >>= fun stroke_width ->
        return (Some{
          is_fixed_pitch;
          italic_angle;
          underline_position;
          underline_thickness;
          paint_type;
          charstring_type;
          font_bbox;
          stroke_width;
        })
