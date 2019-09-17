
open OtfTypes
open OtfUtils
open OtfDecBasic


let reverse_some opts =
  let rec aux acc opts =
    match opts with
    | []              -> Some(acc)
    | None :: _       -> None
    | Some(x) :: tail -> aux (x :: acc) tail
  in
  aux [] opts


let make_cff (dcff : OtfDecCFF.cff_decoder) (gidlst : glyph_id list) =
  let decoder = Otfm.CFF(dcff) in

(* -- generates the subset of the glyph table -- *)
  gidlst |> List.fold_left (fun res gid ->
    res >>= fun rgacc ->
    OtfDecCFF.get_cff_raw_glyph dcff gid >>= fun rg ->
    return (rg :: rgacc)
  ) (return []) >>= fun rgacc ->
  match reverse_some rgacc with
  | None ->
      return None

  | Some(rglst) ->
      let encf = Otfm.Encode.cff_outline_tables dcff in
      encf rglst >>= fun (info, gdata) ->
      let rawtbl_hmtx = info.Otfm.Encode.hmtx in
      let (glyph_tables, oltype) =
        match gdata with
        | TrueTypeGlyph(rawtbl_glyf, rawtbl_loca) ->
            ([rawtbl_glyf; rawtbl_loca], Otfm.Encode.TrueTypeOutline)

        | CFFGlyph(rawtbl_cff) ->
            ([rawtbl_cff]              , Otfm.Encode.CFFData)
      in

    (* -- updates the 'maxp' table -- *)
      Otfm.maxp decoder >>= fun maxp ->
      let maxpnew =
        { maxp with
          Otfm.maxp_num_glyphs = info.Otfm.Encode.number_of_glyphs;
        }
      in
      Otfm.Encode.maxp oltype maxpnew >>= fun rawtbl_maxp ->

    (* -- updates the 'head' table -- *)
      Otfm.head decoder >>= fun head ->
      let headnew =
        { head with
          head_xmin                = info.Otfm.Encode.xmin;
          head_ymin                = info.Otfm.Encode.ymin;
          head_xmax                = info.Otfm.Encode.xmax;
          head_ymax                = info.Otfm.Encode.ymax;
          head_index_to_loc_format = info.Otfm.Encode.index_to_loc_format;
        }
      in
      Otfm.Encode.head headnew >>= fun rawtbl_head ->

    (* -- updates the 'hhea' table -- *)
      Otfm.hhea decoder >>= fun hhea ->
      let hheanew =
        { hhea with
          hhea_advance_width_max      = info.Otfm.Encode.advance_width_max;
          hhea_min_left_side_bearing  = info.Otfm.Encode.min_left_side_bearing;
          hhea_min_right_side_bearing = info.Otfm.Encode.min_right_side_bearing;
          hhea_xmax_extent            = info.Otfm.Encode.x_max_extent;
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
      Otfm.Encode.make_font_file oltype (common_tables @ glyph_tables) >>= fun data ->
      return (Some(data))
