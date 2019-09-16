
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
