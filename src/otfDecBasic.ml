
open OtfTypes
open OtfUtils

module Tag = OtfTag

type tag = Tag.t

type error = OtfError.t
type error_ctx = OtfError.ctx

type 'a ok = ('a, error) result

type src = [ `String of string ]

type decoder_state =
  | Fatal of error
  | Start
  | Ready

type 'a cache =
  | Uncached
  | Cached of 'a

type decoder =
  {
    i                           : string;                     (* input data.                *)
    i_max                       : int;                        (* input maximal position.    *)
    mutable i_pos               : int;                        (* input current position.    *)
    mutable state               : decoder_state;              (* decoder state.             *)
    mutable ctx                 : error_ctx;                  (* the current error context. *)
    mutable flavour             : flavour;                    (* decoded flavour.           *)
    mutable tables              : ((tag * int * int) list) cache;     (* decoded table records.     *)
    mutable loca_pos_and_format : ((int * loc_format) option) cache;  (* for TTF fonts, lazy init.  *)
    mutable glyf_pos            : (int option) cache;         (* for TTF fonts, lazy init.  *)
    mutable buf                 : Buffer.t;                   (* internal buffer.           *)
  }

type ttc_element = int * decoder

type decoder_scheme =
  | SingleDecoder      of decoder
  | TrueTypeCollection of ttc_element list


let decoder_src d = `String(d.i)

let err_eoi d                = err (`Unexpected_eoi(d.ctx))
let e_version d v            = `Unknown_version(d.ctx, v)
let err_version d v          = err (e_version d v)
let err_loca_format d v      = err (`Unknown_loca_format(d.ctx, v))
let err_composite_format d v = err (`Unknown_composite_format(d.ctx, v))
let err_coverage_length d    = err (`Inconsistent_length_of_coverage(d.ctx))
let err_fatal d e            = begin d.state <- Fatal(e); err e end
let set_ctx d ctx            = begin d.ctx <- ctx; end

let miss d count = d.i_max - d.i_pos + 1 < count
let cur_pos d = d.i_pos
let seek_pos pos d =
  if pos > d.i_max then
    err (`Invalid_offset(d.ctx, pos))
  else begin
    d.i_pos <- pos;
    return ()
  end


let get_table_list d =
  match d.tables with
  | Uncached       -> assert false  (* -- must be initialized first -- *)
  | Cached(tables) -> tables


let seek_table tag d () =
  let tables = get_table_list d in
  match List.find_opt (fun (t, _, _) -> tag = t) tables with
  | Some((_, pos, len)) ->
      if pos > d.i_max then
        err (`Invalid_offset(`Table(tag), pos))
      else begin
        set_ctx d (`Table(tag));
        d.i_pos <- pos;
        return (Some(len))
      end

  | None ->
      return None


let seek_required_table tag d () =
  seek_table tag d () >>= function
  | Some(_) -> return ()
  | None    -> err (`Missing_required_table(tag))

let d_skip len d =
  if miss d len then err_eoi d else begin
    d.i_pos <- d.i_pos + len;
    return ()
  end

let raw_byte d =
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1;
  unsafe_byte d.i j

let d_bytes len d =
  if miss d len then err_eoi d else begin
    let start = d.i_pos in
    d.i_pos <- d.i_pos + len;
    return (String.sub d.i start len)
  end

let d_uint8 d =
  if miss d 1 then err_eoi d else
    return (raw_byte d)

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
  let open WideInt in
    if miss d 4 then err_eoi d else
      let b0 = !% (raw_byte d) in
      let b1 = !% (raw_byte d) in
      let b2 = !% (raw_byte d) in
      let b3 = !% (raw_byte d) in
      return ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)


let d_uint32_int d =
  if miss d 4 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let s0 = (b0 lsl 8) lor b1 in
    let s1 = (b2 lsl 8) lor b3 in
    return ((s0 lsl 16) lor s1)


let d_int32 d =
  let open WideInt in
    d_uint32 d >>= fun i ->
    return (if i > of_int64 0x7FFFFFFFL then i -% of_int64 0x10000000L else i)


let d_time d =                       (* -- LONGDATETIME as a unix time stamp. -- *)
  if miss d 8 then
    err_eoi d
  else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let b4 = raw_byte d in let b5 = raw_byte d in
    let b6 = raw_byte d in let b7 = raw_byte d in
    let s0 = !% ((b0 lsl 8) lor b1) in
    let s1 = !% ((b2 lsl 8) lor b3) in
    let s2 = !% ((b4 lsl 8) lor b5) in
    let s3 = !% ((b6 lsl 8) lor b7) in
    let v = WideInt.((s0 lsl 48) lor (s1 lsl 32) lor (s2 lsl 16) lor s3) in
    let unix_epoch = !%% 2_082_844_800L (* -- in seconds since 1904-01-01 00:00:00 -- *) in
    return (v -% unix_epoch)


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

let d_utf_16be len (* -- in bytes -- *) d =            (* -- returns an UTF-8 string. -- *)
  let rec add_utf_8 b i = function
    | `Malformed(_) -> add_utf_8 b i (`Uchar(Uutf.u_rep))
    | `Uchar(u)     -> begin Uutf.Buffer.add_utf_8 b u; b end
  in
  d_bytes len d >>= fun s ->
  Buffer.clear d.buf;
  return (Buffer.contents (Uutf.String.fold_utf_16be add_utf_8 d.buf s))


let d_device_table (d : decoder) : device_table ok =
  d_uint16 d >>= fun startSize ->
  d_uint16 d >>= fun endSize ->
  d_uint16 d >>= fun deltaFormat ->
  d_uint16 d >>= fun deltaValue ->
  return (startSize, endSize, deltaFormat, deltaValue)


let confirm_version d version versionreq =
  if version <> versionreq then err_version d versionreq else return ()

let d_repeat n df d =
  let rec aux acc i =
    if i <= 0 then return (Alist.to_list acc) else
    df d >>= fun data ->
    aux (Alist.extend acc data) (i - 1)
  in
  aux Alist.empty n


let d_list df d =
  d_uint16 d >>= fun count ->
  Format.fprintf fmtgen "(d_list) count = %d@," count;
  d_repeat count df d


let d_list_filtered df predicate d =
  let rec aux acc imax i =
    if i >= imax then return (Alist.to_list acc) else
    df d >>= fun data ->
    if predicate i then
      aux (Alist.extend acc data) imax (i + 1)
    else
      aux acc imax (i + 1)
  in
    d_uint16 d >>= fun count ->
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


let d_offset_list (offset_origin : int) d : (int list) ok =
  d_list d_uint16 d >>= fun reloffsetlst ->
  return (reloffsetlst |> List.map (fun reloffset -> offset_origin + reloffset))


let d_offset (offset_origin : int) d : int ok =
  d_uint16 d >>= fun reloffset ->
  return (offset_origin + reloffset)


let d_long_offset_list d : (int list) ok =
  d_uint32_int d >>= fun count ->
  d_repeat count d_uint32_int d


let d_offset_opt (offset_origin : int) d : (int option) ok =
  d_uint16 d >>= fun reloffset ->
  if reloffset = 0 then
    return None
  else
    return (Some(offset_origin + reloffset))


let d_fetch offset_origin df d =
  let pos_before = cur_pos d in
  Format.fprintf fmtgen "(d_fetch) | pos_before = %d@," pos_before;
  d_offset offset_origin d >>= fun offset ->
  Format.fprintf fmtgen "          | rel_offset = %d@," (offset - offset_origin);
  Format.fprintf fmtgen "          | offset     = %d@," offset;
  seek_pos offset d >>= fun () ->
  df d >>= fun res ->
  seek_pos (pos_before + 2) d >>= fun () ->
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
let rec d_table_records (d : decoder) (count : int) : unit ok =
  let rec aux tableacc count =
    if count = 0 then begin
      d.state <- Ready;
      d.tables <- Cached(Alist.to_list tableacc);
      return ()
    end else
      d_uint32     d >>= fun tagwint ->
      d_skip 4     d >>= fun () ->
      d_uint32_int d >>= fun off ->
      d_uint32_int d >>= fun len ->
      let table = (Tag.of_wide_int tagwint, off, len) in
      aux (Alist.extend tableacc table) (count - 1)
  in
  aux Alist.empty count


let d_version is_ttc_element d =
  d_uint32 d >>= fun tagwint ->
    match OtfTag.of_wide_int tagwint with
    | t  when t = Tag.v_OTTO -> begin d.flavour <- CFF     ; return true end
    | t  when t = Tag.v_true -> begin d.flavour <- TTF_true; return true end
    | t  when t = Tag.v_1_0  -> begin d.flavour <- TTF_OT  ; return true end
    | t  when t = Tag.v_ttcf -> if is_ttc_element then err `Layered_ttc else return false
    | t                      -> err (`Unknown_flavour(t))


let d_structure d =                   (* -- offset table and table directory. -- *)
  d_uint16       d >>= fun numTables ->
  d_skip (3 * 2) d >>= fun () ->
  set_ctx d `Table_directory;
  d_table_records d numTables


let d_ttc_header d : (ttc_element list) ok =
  d_uint32 d >>= function
  | version_ttc  when version_ttc = !%% 0x00010000L || version_ttc = !%% 0x00020000L ->
      d_long_offset_list d >>= fun offsetlst ->
      return (offsetlst |> List.map (fun offset -> (offset, d)))
  | version_ttc ->
      err_version d version_ttc


let decoder src =
  let (i, i_pos, i_max) =
    match src with
    | `String(s) -> (s, 0, String.length s - 1)
  in
  let d =
    { i; i_pos; i_max;
      state = Start;
      ctx = `Offset_table;
      flavour = TTF_OT;    (* dummy initial value *)
      tables = Uncached;
      loca_pos_and_format = Uncached; glyf_pos = Uncached;
      buf = Buffer.create 253; }
  in
  d_version false d >>= fun is_single ->
  if is_single then
    return (SingleDecoder(d))
  else
    d_ttc_header d >>= fun ttc ->
    return (TrueTypeCollection(ttc))


let decoder_of_ttc_element ttcelem =
  let (offset, d) = ttcelem in
  let delem =
    { i = d.i;  i_pos = d.i_pos;  i_max = d.i_max;
      state = Start;
      ctx = `Offset_table;
      flavour = d.flavour;
      tables = d.tables;
      loca_pos_and_format = d.loca_pos_and_format;
      glyf_pos = d.glyf_pos;
      buf = Buffer.create 253; }
  in
  seek_pos offset delem >>= fun () ->
  d_version true delem >>= fun _ ->
  return delem


let init_decoder d =
  match d.state with
  | Ready ->
      d.ctx <- `Table_directory;
      return ()

  | Fatal(e) ->
      err e

  | Start ->
      begin
        match d_structure d with
        | Ok(()) as ok -> ok
        | Error(e)     -> err_fatal d e
      end


let init_glyf d () : (int option) ok =
  match d.glyf_pos with
  | Cached(posopt) ->
      return posopt

  | Uncached ->
      begin
        seek_table Tag.glyf d () >>= fun lenopt ->
        let posopt =
          match lenopt with
          | None    -> None
          | Some(_) -> Some(d.i_pos)
        in
        d.glyf_pos <- Cached(posopt);
        return posopt
      end


let d_loca_format d () =
  d_uint16 d >>= function
  | 0 -> return ShortLocFormat
  | 1 -> return LongLocFormat
  | i -> err_loca_format d i


let init_loca d () : ((int * loc_format) option) ok =
  match d.loca_pos_and_format with
  | Cached(locaopt) ->
      return locaopt

  | Uncached ->
      seek_required_table Tag.head d () >>= fun () ->
      d_skip 50 d >>=
      d_loca_format d >>= fun locfmt ->
      seek_required_table Tag.loca d () >>= fun () ->
      let pos = d.i_pos in
      let pair = (pos, locfmt) in
      d.loca_pos_and_format <- Cached(Some(pair));
      return (Some(pair))


let flavour d =
    init_decoder d >>= fun () ->
    return (d.flavour)


let table_list d =
  let tables = get_table_list d in
  let tags d = List.rev_map (fun (t, _, _) -> t) tables in
  init_decoder d >>= fun () -> return (tags d)


let table_mem d tag =
  let tables = get_table_list d in
  let exists_tag tag d = List.exists (fun (t, _, _) -> tag = t) tables in
  init_decoder d >>= fun () -> return (exists_tag tag d)


let table_raw d tag =
  init_decoder   d >>=
  seek_table tag d >>= function
  | None      -> return None
  | Some(len) -> d_bytes len d >>= fun bytes -> return (Some(bytes))


(* -- convenience -- *)

let glyph_count d =
  init_decoder d >>=
  seek_required_table Tag.maxp d >>= fun () ->
  d_skip 4 d >>= fun () ->
  d_uint16 d >>= fun count ->
  return count


let postscript_name d = (* -- rigorous postscript name lookup, see OT spec p. 39 -- *)
  init_decoder d >>=
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
