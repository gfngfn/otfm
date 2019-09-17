
open OtfTypes
open OtfUtils

module Tag = OtfTag

type tag = Tag.t

type error = OtfError.t
type error_context = OtfError.context

type 'a ok = ('a, error) result

type source = [ `String of string ]

type decoder_state =
  | Fatal of error
  | Start
  | Ready

type table_record = {
  table_tag    : tag;
  table_offset : int;
  table_length : int;
}

type common_decoder =
  {
    i                           : string;                     (* input data.                *)
    i_max                       : int;                        (* input maximal position.    *)
    mutable i_pos               : int;                        (* input current position.    *)
    mutable state               : decoder_state;              (* decoder state.             *)
    mutable context             : error_context;                 (* the current error context. *)
    mutable tables              : (table_record list) cache;     (* decoded table records.     *)
    mutable buf                 : Buffer.t;                   (* internal buffer.           *)
  }


let make_initial_decoder (src : source) : common_decoder =
  let (i, i_pos, i_max) =
    match src with
    | `String(s) -> (s, 0, String.length s - 1)
  in
  { i; i_pos; i_max;
    state = Start;
    context = `Offset_table;
    tables = Uncached;
    buf = Buffer.create 253; }


let decoder_source (d : common_decoder) : source =
  `String(d.i)


let copy_and_initialize_decoder (d : common_decoder) : common_decoder =
  make_initial_decoder (decoder_source d)


let err_eoi (cd : common_decoder) : 'a ok = err (`Unexpected_eoi(cd.context))

let e_version (cd : common_decoder) (v : wint) : error = `Unknown_version(cd.context, v)

let err_version (cd : common_decoder) (v : wint) : 'a ok = err (e_version cd v)

let err_loca_format cd v      = err (`Unknown_loca_format(cd.context, v))
let err_composite_format cd v = err (`Unknown_composite_format(cd.context, v))
let err_coverage_length cd    = err (`Inconsistent_length_of_coverage(cd.context))
let err_fatal cd e            = begin cd.state <- Fatal(e); err e end


let set_context (cd : common_decoder) (ctx : error_context) : unit =
  cd.context <- ctx


let miss (cd : common_decoder) (count : int) : bool =
  cd.i_max - cd.i_pos + 1 < count


let cur_pos (cd : common_decoder) : int =
  cd.i_pos


let seek_pos (pos : int) (cd : common_decoder) : unit ok =
  if pos > cd.i_max then
    err (`Invalid_offset(cd.context, pos))
  else begin
    cd.i_pos <- pos;
    return ()
  end


let get_table_list (cd : common_decoder) =
  match cd.tables with
  | Uncached       -> assert false  (* -- must be initialized first -- *)
  | Cached(tables) -> tables


(* --
   `seek_table tag cd`
     moves to the beginning of the table corresponding to `tag`
     and returns its length if the table exists,
     or does nothing otherwise.
   -- *)
let seek_table (tag : tag) (cd : common_decoder) : (int option) ok =
  let tables = get_table_list cd in
  match List.find_opt (fun table -> tag = table.table_tag) tables with
  | Some(table) ->
      let pos = table.table_offset in
      if pos > cd.i_max then
        err (`Invalid_offset(`Table(tag), pos))
      else begin
        set_context cd (`Table(tag));
        cd.i_pos <- pos;
        return (Some(table.table_length))
      end

  | None ->
      return None


let seek_required_table (tag : tag) (cd : common_decoder) : unit ok =
  seek_table tag cd >>= function
  | Some(_) -> return ()
  | None    -> err (`Missing_required_table(tag))


let d_skip (len : int) (cd : common_decoder) : unit ok =
  if miss cd len then err_eoi cd else begin
    cd.i_pos <- cd.i_pos + len;
    return ()
  end


let raw_byte (cd : common_decoder) : int =
  let j = cd.i_pos in
  cd.i_pos <- cd.i_pos + 1;
  unsafe_byte cd.i j


let d_bytes (len : int) (cd : common_decoder) : string ok =
  if miss cd len then err_eoi cd else begin
    let start = cd.i_pos in
    cd.i_pos <- cd.i_pos + len;
    return (String.sub cd.i start len)
  end


let d_uint8 (cd : common_decoder) : int ok =
  if miss cd 1 then err_eoi cd else
    return (raw_byte cd)


let d_int8 (cd : common_decoder) : int ok =
  d_uint8 cd >>= fun i ->
  return (if i > 0x7F then i - 0x100 else i)


let d_uint16 (cd : common_decoder) : int ok =
  if miss cd 2 then err_eoi cd else
    let b0 = raw_byte cd in
    let b1 = raw_byte cd in
    return ((b0 lsl 8) lor b1)


let d_int16 (cd : common_decoder) : int ok =
  d_uint16 cd >>= fun i ->
  return (if i > 0x7FFF then i - 0x10000 else i)


let d_uint24 (cd : common_decoder) : int ok =
  if miss cd 3 then err_eoi cd else
    let b0 = raw_byte cd in
    let b1 = raw_byte cd in
    let b2 = raw_byte cd in
    return ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let d_uint32 (cd : common_decoder) : wint ok =
  let open WideInt in
    if miss cd 4 then err_eoi cd else
      let b0 = !% (raw_byte cd) in
      let b1 = !% (raw_byte cd) in
      let b2 = !% (raw_byte cd) in
      let b3 = !% (raw_byte cd) in
      return ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)


let d_uint32_int (cd : common_decoder) : int ok =
  if miss cd 4 then err_eoi cd else
    let b0 = raw_byte cd in let b1 = raw_byte cd in
    let b2 = raw_byte cd in let b3 = raw_byte cd in
    let s0 = (b0 lsl 8) lor b1 in
    let s1 = (b2 lsl 8) lor b3 in
    return ((s0 lsl 16) lor s1)


let d_int32 (cd : common_decoder) : wint ok =
  let open WideInt in
    d_uint32 cd >>= fun i ->
    return (if i > of_int64 0x7FFFFFFFL then i -% of_int64 0x10000000L else i)


(* -- LONGDATETIME as a unix time stamp. -- *)
let d_time (cd : common_decoder) : wint ok =
  if miss cd 8 then
    err_eoi cd
  else
    let b0 = raw_byte cd in let b1 = raw_byte cd in
    let b2 = raw_byte cd in let b3 = raw_byte cd in
    let b4 = raw_byte cd in let b5 = raw_byte cd in
    let b6 = raw_byte cd in let b7 = raw_byte cd in
    let s0 = !% ((b0 lsl 8) lor b1) in
    let s1 = !% ((b2 lsl 8) lor b3) in
    let s2 = !% ((b4 lsl 8) lor b5) in
    let s3 = !% ((b6 lsl 8) lor b7) in
    let v = WideInt.((s0 lsl 48) lor (s1 lsl 32) lor (s2 lsl 16) lor s3) in
    let unix_epoch = !%% 2_082_844_800L (* -- in seconds since 1904-01-01 00:00:00 -- *) in
    return (v -% unix_epoch)


let d_fixed (cd : common_decoder) : (int32 * int32) ok =
  if miss cd 4 then err_eoi cd else
    let b0 = raw_byte cd in let b1 = raw_byte cd in
    let b2 = raw_byte cd in let b3 = raw_byte cd in
    let s0 = Int32.of_int ((b0 lsl 8) lor b1) in
    let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
    return (s0, s1)


let d_f2dot14 (cd : common_decoder) : float ok =
  d_int16 cd >>= fun v ->
  return ((float v) /. 16384.0)


(* -- len: in bytes -- *)
(* -- returns an UTF-8 string. -- *)
let d_utf_16be (len : int) (cd : common_decoder) : string ok =
  let rec add_utf_8 b i = function
    | `Malformed(_) -> add_utf_8 b i (`Uchar(Uutf.u_rep))
    | `Uchar(u)     -> begin Uutf.Buffer.add_utf_8 b u; b end
  in
  d_bytes len cd >>= fun s ->
  Buffer.clear cd.buf;
  return (Buffer.contents (Uutf.String.fold_utf_16be add_utf_8 cd.buf s))


let d_device_table (cd : common_decoder) : device_table ok =
  d_uint16 cd >>= fun startSize ->
  d_uint16 cd >>= fun endSize ->
  d_uint16 cd >>= fun deltaFormat ->
  d_uint16 cd >>= fun deltaValue ->
  return (startSize, endSize, deltaFormat, deltaValue)


let confirm_version (cd : common_decoder) (version : wint) (versionreq : wint) : unit ok =
  if version <> versionreq then
    err_version cd versionreq
  else
    return ()


let d_repeat (type a) (n : int) (cdf : common_decoder -> a ok) (cd : common_decoder) : (a list) ok =
  let rec aux acc i =
    if i <= 0 then return (Alist.to_list acc) else
    cdf cd >>= fun data ->
    aux (Alist.extend acc data) (i - 1)
  in
  aux Alist.empty n


let d_list (type a) (cdf : common_decoder -> a ok) (cd : common_decoder) : (a list) ok =
  d_uint16 cd >>= fun count ->
  Format.fprintf fmtgen "(d_list) count = %d@," count;
  d_repeat count cdf cd


let d_list_filtered (type a) (cdf : common_decoder -> a ok) (predicate : int -> bool) (cd : common_decoder) : (a list) ok =
  let rec aux acc imax i =
    if i >= imax then return (Alist.to_list acc) else
    cdf cd >>= fun data ->
    if predicate i then
      aux (Alist.extend acc data) imax (i + 1)
    else
      aux acc imax (i + 1)
  in
    d_uint16 cd >>= fun count ->
    Format.fprintf fmtgen "(d_list_filtered) count = %d@," count;
    aux Alist.empty count 0


let d_list_access df ireq d =
  let rec aux imax i =
    if i >= imax then return None else
    df d >>= fun data ->
    if i = ireq then
      return (Some(data))
    else
      aux imax (i + 1)
  in
    d_uint16 d >>= fun count ->
    aux count 0


let d_offset_list (offset_origin : int) (cd : common_decoder) : (int list) ok =
  d_list d_uint16 cd >>= fun reloffsetlst ->
  return (reloffsetlst |> List.map (fun reloffset -> offset_origin + reloffset))


let d_offset (offset_origin : int) (cd : common_decoder) : int ok =
  d_uint16 cd >>= fun reloffset ->
  return (offset_origin + reloffset)


let d_long_offset_list (cd : common_decoder) : (int list) ok =
  d_uint32_int cd >>= fun count ->
  d_repeat count d_uint32_int cd


let d_offset_opt (offset_origin : int) (cd : common_decoder) : (int option) ok =
  d_uint16 cd >>= fun reloffset ->
  if reloffset = 0 then
    return None
  else
    return (Some(offset_origin + reloffset))


let d_fetch (type a) (offset_origin : int) (cdf : common_decoder -> a ok) (cd : common_decoder) : a ok =
  let pos_before = cur_pos cd in
  Format.fprintf fmtgen "(d_fetch) | pos_before = %d@," pos_before;
  d_offset offset_origin cd >>= fun offset ->
  Format.fprintf fmtgen "          | rel_offset = %d@," (offset - offset_origin);
  Format.fprintf fmtgen "          | offset     = %d@," offset;
  seek_pos offset cd >>= fun () ->
  cdf cd >>= fun res ->
  seek_pos (pos_before + 2) cd >>= fun () ->
  return res


let d_fetch_opt offset_origin df d =
  let pos_before = cur_pos d in
  d_offset_opt offset_origin d >>= function
  | None ->
      Format.fprintf fmtgen "(d_fetch_opt) | pos_before = %d@," pos_before;
      Format.fprintf fmtgen "              | NULL@,";
      seek_pos (pos_before + 2) d >>= fun () ->
      return None

  | Some(offset) ->
      Format.fprintf fmtgen "(d_fetch_opt) | pos_before = %d@," pos_before;
      Format.fprintf fmtgen "              | non-NULL@,";
      seek_pos offset d >>= fun () ->
      df d >>= fun res ->
      seek_pos (pos_before + 2) d >>= fun () ->
      return (Some(res))


let d_fetch_list offset_origin df d =
  let pos_before = cur_pos d in
  Format.fprintf fmtgen "(d_fetch_list) | pos_before = %d@," pos_before;
  d_offset_opt offset_origin d >>= function
  | None ->
      Format.fprintf fmtgen "               | NULL@,";
      seek_pos (pos_before + 2) d >>= fun () ->
      return []

  | Some(offset) ->
      Format.fprintf fmtgen "               | non-NULL@,";
      seek_pos offset d >>= fun () ->
      df d >>= fun lst ->
      seek_pos (pos_before + 2) d >>= fun () ->
      return lst


let d_fetch_long offset_origin df d =
  let pos_before = cur_pos d in
  d_uint32_int d >>= fun reloffset ->
  let offset = offset_origin + reloffset in
  seek_pos offset d >>= fun () ->
  df d >>= fun res ->
  seek_pos (pos_before + 4) d >>= fun () ->
  return (offset, res)


(* --
   Assuming that the current position is immediately before the first table record,
   `d_table_records` reads all of the table records and caches them in `d.tables`.
   -- *)
let rec d_table_records (cd : common_decoder) (count : int) : unit ok =
  let rec aux tableacc count =
    if count = 0 then begin
      cd.state <- Ready;
      cd.tables <- Cached(Alist.to_list tableacc);
      return ()
    end else
      d_uint32     cd >>= fun tagwint ->
      d_skip 4     cd >>= fun () ->
      d_uint32_int cd >>= fun off ->
      d_uint32_int cd >>= fun len ->
      let table =
        {
          table_tag    = Tag.of_wide_int tagwint;
          table_offset = off;
          table_length = len;
        }
      in
      aux (Alist.extend tableacc table) (count - 1)
  in
  aux Alist.empty count


(* -- offset table and table directory. -- *)
let d_structure (cd : common_decoder) =
  d_uint16       cd >>= fun numTables ->
  d_skip (3 * 2) cd >>= fun () ->
  set_context cd `Table_directory;
  d_table_records cd numTables


(* --
  `d_ttc_header_offset_list`
    returns the list of offsets of TTC elements.
   -- *)
let d_ttc_header_offset_list (cd : common_decoder) : (int list) ok =
  d_uint32 cd >>= function
  | version_ttc  when version_ttc = !%% 0x00010000L || version_ttc = !%% 0x00020000L ->
      d_long_offset_list cd

  | version_ttc ->
      err_version cd version_ttc


let init_decoder (cd : common_decoder) : unit ok =
  match cd.state with
  | Ready ->
      cd.context <- `Table_directory;
      return ()

  | Fatal(e) ->
      err e

  | Start ->
      begin
        match d_structure cd with
        | Ok(()) as ok -> ok
        | Error(e)     -> err_fatal cd e
      end


let table_list (cd : common_decoder) : (tag list) ok =
  init_decoder cd >>= fun () ->
  let tables = get_table_list cd in
  return (tables |> List.rev_map (fun table -> table.table_tag))


let table_mem (cd : common_decoder) (tag : tag) : bool ok =
  init_decoder cd >>= fun () ->
  let tables = get_table_list cd in
  return (tables |> List.exists (fun table -> tag = table.table_tag))


let table_raw (cd : common_decoder) (tag : tag) : (string option) ok =
  init_decoder cd >>= fun () ->
  seek_table tag cd >>= function
  | None      -> return None
  | Some(len) -> d_bytes len cd >>= fun bytes -> return (Some(bytes))


(* -- convenience -- *)

let glyph_count (cd : common_decoder) : int ok =
  init_decoder cd >>= fun () ->
  seek_required_table Tag.maxp cd >>= fun () ->
  d_skip 4 cd >>= fun () ->
  d_uint16 cd >>= fun count ->
  return count


(* -- rigorous postscript name lookup, see OT spec p. 39 -- *)
let postscript_name d =
  init_decoder d >>= fun () ->
  seek_required_table Tag.name d >>= fun () ->
  let pos_name = cur_pos d in
  d_uint16 d >>= fun version ->
  if version > 1 then err_version d (!% version) else
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
      seek_pos (pos_name + soff + off) d >>= fun () ->
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


let get_uint16 data offset =
  let b0 = Char.code (String.get data offset) in
  let b1 = Char.code (String.get data (offset + 1)) in
  (b0 lsl 8) lor b1


let get_int16 data i =
  let ui = get_uint16 data i in
  if ui >= 0x8000 then ui - 0x10000 else ui


let d_hm_count (cd : common_decoder) : int ok =
  seek_required_table Tag.hhea cd >>= fun () ->
  d_skip (4 + 15 * 2) cd >>= fun () ->
  d_uint16            cd >>= fun hm_count ->
  return hm_count


let hmtx_single (d : common_decoder) (gid : glyph_id) =
  glyph_count d >>= fun glyph_count ->
  d_hm_count  d >>= fun hm_count ->
  seek_required_table Tag.hmtx d >>= fun () ->
  if gid < hm_count then
    d_skip (4 * gid) d >>= fun () ->
    d_uint16 d >>= fun aw ->
    d_int16  d >>= fun lsb ->
    return (aw, lsb)
  else
    d_skip (4 * (hm_count - 1)) d >>= fun () ->
    d_uint16 d >>= fun aw ->
    d_skip 2 d >>= fun () ->
    d_skip (2 * (gid - hm_count)) d >>= fun () ->
    d_int16  d >>= fun lsb ->
    return (aw, lsb)
