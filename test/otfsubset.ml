

let ( >>= ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e :> [ Otfm.error | `Msg of string ])

let return v = Ok(v)

let err e = Error(e)


let string_of_file inf =
  try
    let ic = open_in_bin inf in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in
    let s = Bytes.create buf_size in
    try
      while true do
        let c = input ic s 0 buf_size in
        if c = 0 then raise Exit else
        Buffer.add_substring b (Bytes.unsafe_to_string s) 0 c
      done;
      assert false
    with
    | Exit -> close_in ic; return (Buffer.contents b)
    | Failure _ -> close_in ic; err (`Msg (Format.sprintf "%s: input file too large" inf))
    | Sys_error e -> close_in ic; err (`Msg e);
  with
  | Sys_error e -> (Error (`Msg e))


let ( -- ) i j =
  let rec aux acc i j =
    if i >= j then List.rev acc else
      aux (i :: acc) (i + 1) j
  in
  aux [] i j


let main () =
  let gidlst = List.concat [0 -- 3; 20 -- 50; 100 -- 120] in
  let srcinref = ref None in
  let srcoutref = ref "a.ttf" in
  let specify_in src = srcinref := Some(src) in
  let specify_out src = srcoutref := src in
  let options =
    [
      ("-o", Arg.String(specify_out), "specify output file");
    ]
  in
  Arg.parse (Arg.align options) specify_in "";
  let srcout = !srcoutref in
  begin
    match !srcinref with
    | None      -> err (`Msg "no input file specified")
    | Some(src) -> string_of_file src
  end >>= fun s ->
  begin
    Otfm.decoder (`String(s)) >>= function
    | Otfm.SingleDecoder(d)      -> return d
    | Otfm.TrueTypeCollection(_) -> err (`Msg "unsupported TTC")
  end >>= fun d ->

(* -- generates the subset of the glyph table -- *)
  gidlst |> List.fold_left (fun res gid ->
    res >>= fun rgacc ->
    Otfm.get_raw_glyph d gid >>= fun rg ->
    return (rg :: rgacc)
  ) (return []) >>= fun rgacc ->
  let rglst = List.rev rgacc in
  Otfm.Encode.truetype_outline_tables rglst >>= fun info ->
  let rawtbl_hmtx = info.Otfm.Encode.hmtx in
  let rawtbl_glyf = info.Otfm.Encode.glyf in
  let rawtbl_loca = info.Otfm.Encode.loca in

(* -- updates the 'maxp' table -- *)
  Otfm.maxp d >>= fun maxp ->
  let maxpnew =
    { maxp with
      Otfm.maxp_num_glyphs = info.Otfm.Encode.number_of_glyphs;
    }
  in
  Otfm.Encode.maxp maxpnew >>= fun rawtbl_maxp ->

(* -- updates the 'head' table -- *)
  Otfm.head d >>= fun head ->
  let headnew =
    { head with
      Otfm.head_xmin = info.Otfm.Encode.xmin;
      Otfm.head_ymin = info.Otfm.Encode.ymin;
      Otfm.head_xmax = info.Otfm.Encode.xmax;
      Otfm.head_ymax = info.Otfm.Encode.ymax;
      Otfm.head_index_to_loc_format = info.Otfm.Encode.index_to_loc_format;
    }
  in
  Otfm.Encode.head headnew >>= fun rawtbl_head ->

(* -- updates the 'hhea' table -- *)
  Otfm.hhea d >>= fun hhea ->
  let hheanew =
    { hhea with
      Otfm.hhea_advance_width_max = info.Otfm.Encode.advance_width_max;
      Otfm.hhea_min_left_side_bearing = info.Otfm.Encode.min_left_side_bearing;
      Otfm.hhea_min_right_side_bearing = info.Otfm.Encode.min_right_side_bearing;
      Otfm.hhea_xmax_extent = info.Otfm.Encode.x_max_extent;
    }
  in
  Otfm.Encode.hhea info.Otfm.Encode.number_of_h_metrics hheanew >>= fun rawtbl_hhea ->

(* -- 'cmap' table -- *)
  Otfm.Encode.empty_cmap () >>= fun rawtbl_cmap ->

  Otfm.Encode.make_font_file [
    rawtbl_cmap;
    rawtbl_head;
    rawtbl_hhea;
    rawtbl_maxp;
    rawtbl_hmtx;
    rawtbl_loca;
    rawtbl_glyf;
  ] >>= fun data ->
  let fout = open_out srcout in
  output_string fout data;
  close_out fout;

  return ()


let _ =
  let res = main () in
  match res with
  | Error(`Msg(s))             -> print_endline s; exit 1
  | Error(#Otfm.error as oerr) -> Format.printf "%a\n" Otfm.pp_error oerr; exit 1
  | Ok(())                     -> print_endline "finished"
