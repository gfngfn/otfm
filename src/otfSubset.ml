
open OtfUtils


let reverse_some opts =
  let rec aux acc opts =
    match opts with
    | []              -> Some(acc)
    | None :: _       -> None
    | Some(x) :: tail -> aux (x :: acc) tail
  in
  aux [] opts


let make d cffinfo gidlst =

(* -- generates the subset of the glyph table -- *)
  gidlst |> List.fold_left (fun res gid ->
    res >>= fun rgacc ->
    Otfm.get_raw_glyph d cffinfo gid >>= fun rg ->
    return (rg :: rgacc)
  ) (return []) >>= fun rgacc ->
  match reverse_some rgacc with
  | None ->
      return None

  | Some(rglst) ->
      let encf =
        match cffinfo with
        | None          -> Otfm.Encode.truetype_outline_tables
        | Some(cffinfo) -> Otfm.Encode.cff_outline_tables d cffinfo
      in
      encf rglst >>= fun (info, gdata) ->
      let rawtbl_hmtx = info.Otfm.Encode.hmtx in
      let glyph_tables =
        match gdata with
        | TrueTypeGlyph(rawtbl_glyf, rawtbl_loca) ->
            [rawtbl_glyf; rawtbl_loca]
        | CFFGlyph(rawtbl_cff) ->
            [rawtbl_cff]
      in

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

      let common_tables =
        [
          rawtbl_cmap;
          rawtbl_head;
          rawtbl_hhea;
          rawtbl_maxp;
          rawtbl_hmtx;
        ]
      in
      Otfm.Encode.make_font_file (common_tables @ glyph_tables) >>= fun data ->
      return (Some(data))
