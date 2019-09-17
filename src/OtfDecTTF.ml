
open OtfTypes
open OtfUtils
open OtfDecBasic
module Tag = OtfTag


type ttf_specific = {
  mutable loca_pos_and_format : (int * loc_format) cache;  (* for TTF fonts, lazy init.  *)
  mutable glyf_pos            : int cache;                 (* for TTF fonts, lazy init.  *)
}

type ttf_decoder = {
  ttf_common   : common_decoder;
  ttf_specific : ttf_specific;
}

let ttf_common (dttf : ttf_decoder) : common_decoder =
  dttf.ttf_common


let make_initial_ttf (d : common_decoder) : ttf_decoder =
  {
    ttf_common = d;
    ttf_specific = {
      loca_pos_and_format = Uncached;
      glyf_pos            = Uncached;
    };
  }

let init_glyf (dttf : ttf_decoder) : int ok =
  match dttf.ttf_specific.glyf_pos with
  | Cached(pos) ->
      return pos

  | Uncached ->
      let cd = dttf.ttf_common in
      seek_required_table Tag.glyf cd >>= fun _ ->
      let pos = cur_pos cd in
      dttf.ttf_specific.glyf_pos <- Cached(pos);
      return pos


let d_loca_format (cd : common_decoder) : loc_format ok =
  d_uint16 cd >>= function
  | 0 -> return ShortLocFormat
  | 1 -> return LongLocFormat
  | i -> err_loca_format cd i


let init_loca (dttf : ttf_decoder) : (int * loc_format) ok =
  match dttf.ttf_specific.loca_pos_and_format with
  | Cached(pair) ->
      return pair

  | Uncached ->
      let cd = dttf.ttf_common in
      seek_required_table Tag.head cd >>= fun () ->
      d_skip 50 cd >>= fun () ->
      d_loca_format cd >>= fun locfmt ->
      seek_required_table Tag.loca cd >>= fun () ->
      let pos = cur_pos cd in
      let pair = (pos, locfmt) in
      dttf.ttf_specific.loca_pos_and_format <- Cached(pair);
      return pair


(* -- loca table -- *)

let loca_short pos_loca (cd : common_decoder) (gid : glyph_id) =
  seek_pos (pos_loca + gid * 2) cd >>= fun () ->
  d_uint16 cd >>= fun o1 ->
  d_uint16 cd >>= fun o2 ->
  let o1 = o1 * 2 in
  let o2 = o2 * 2 in
  if o1 = o2 then return None else return (Some(o1, o2 - o1))


let loca_long pos_loca (cd : common_decoder) (gid : glyph_id) =
  seek_pos (pos_loca + gid * 4) cd >>= fun () ->
  d_uint32_int cd >>= fun o1 ->
  d_uint32_int cd >>= fun o2 ->
  if o1 = o2 then return None else return (Some(o1, o2 - o1))


let loca_with_length (dttf : ttf_decoder) (gid : glyph_id) =
  let cd = ttf_common dttf in
  init_decoder cd >>= fun () ->
  init_loca dttf >>= fun (pos_loca, locfmt) ->
    match locfmt with
    | ShortLocFormat -> loca_short pos_loca cd gid
    | LongLocFormat  -> loca_long pos_loca cd gid


let loca (dttf : ttf_decoder) (gid : glyph_id) : (glyf_loc option) ok =
  loca_with_length dttf gid >>= function
  | Some((loc, _)) -> return (Some(loc))
  | None           -> return None


type ttf_raw_glyph = raw_glyph


let ttf_raw_glyph (g : ttf_raw_glyph) = g


let get_composite_offsets (data : string) : ((glyph_id * int) list, error) result =

  let rec loop i acc =
    let flags = get_uint16 data i in
    let i = i + 2 in
    let gid = get_uint16 data i in
    let accnew = Alist.extend acc (gid, i) in
    let i = i + 2 in
    let i = i + (if flags land 1 > 0 then 4 else 2) in
    let d =
      if flags land 8 > 0 then  (* -- scale -- *)
        2
      else if flags land 64 > 0 then  (* -- xy scale -- *)
        4
      else if flags land 128 > 0 then  (* -- m2 -- *)
        8
      else
        0
    in
    if flags land 32 > 0 then
      loop (i + d) accnew
    else
      return (Alist.to_list accnew)
  in

  let numberOfContours = get_int16 data 0 in
  if numberOfContours >= 0 then
    return []
  else
    loop 10 Alist.empty


let get_ttf_raw_glyph (dttf : ttf_decoder) (gid : glyph_id) : (ttf_raw_glyph option) ok =
  let d = ttf_common dttf in
  loca_with_length dttf gid >>= function
  | None ->
      let rawg =
        {
          old_glyph_id = gid;
          glyph_aw = 0;
          glyph_lsb = 0;
          glyph_bbox = (0, 0, 0, 0);
          glyph_data = "";
          glyph_data_length = 0;
          glyph_data_offset = None;
          glyph_composite_offsets = [];
        }
      in
      return (Some(rawg))

  | Some((loc, len)) ->
      init_decoder d >>= fun () ->
      init_glyf dttf >>= fun pos_glyf ->
          let pos = pos_glyf + loc in
          seek_pos pos d >>= fun () ->
          d_int16 d >>= fun _ ->
          d_int16 d >>= fun xmin ->
          d_int16 d >>= fun ymin ->
          d_int16 d >>= fun xmax ->
          d_int16 d >>= fun ymax ->
          seek_pos pos d >>= fun () ->
          d_bytes len d >>= fun data ->
          get_composite_offsets data >>= fun pairlst ->
          hmtx_single d gid >>= fun (aw, lsb) ->
          let rawg =
            {
              old_glyph_id = gid;
              glyph_aw = aw;
              glyph_lsb = lsb;
              glyph_bbox = (xmin, ymin, xmax, ymax);
              glyph_data = data;
              glyph_data_length = len;
              glyph_data_offset = Some(pos);
              glyph_composite_offsets = pairlst;
            }
          in
          return (Some(rawg))
