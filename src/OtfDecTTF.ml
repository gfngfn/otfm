
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
