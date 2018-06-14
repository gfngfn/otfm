
open TestUtil


type position =
  | Single1 of Otfm.glyph_id list * Otfm.value_record
  | Pair1 of Otfm.glyph_id * (Otfm.glyph_id * Otfm.value_record * Otfm.value_record) list
  | Pair2 of Otfm.class_value * (Otfm.class_value * Otfm.value_record * Otfm.value_record) list


let skip gid _ = gid

let f_single acc (gid, gidto) = (gid, [gidto]) :: acc

let f_alt acc (gid, gidaltlst) = (gid, gidaltlst) :: acc

let f_lig acc (gidfst, liginfolst) = (gidfst, liginfolst) :: acc

let f_single1 acc gidlst valrcd = Single1(gidlst, valrcd) :: acc

let f_single2 acc _ =
  Format.printf "<Single2>@,";
  acc

let f_pair1 acc (gidfst, pairinfolst) = Pair1(gidfst, pairinfolst) :: acc

(*
let f_pair2 clsdeflst1 clsdeflst2 lst (clsval, pairinfolst) =
  Pair2(clsval, pairinfolst) :: lst
*)
let f_pair2 _ _ acc _ =
  Format.printf "<Pair2>@,";
  acc

let f_markbase1 _ acc _ _ =
  Format.printf "<MarkBase1>@,";
  acc


let pp_sep fmt () =
  Format.fprintf fmt ",@ "


let decode_gsub scripttag type3tag type4tag d =
  Otfm.gsub_script d >>= fun scriptlst ->

  Format.printf "@[<v2>@,";
  Format.printf "all GSUB Script tags:@ @[[%a]@]@,"
    (Format.pp_print_list ~pp_sep Format.pp_print_string)
      (scriptlst |> List.map Otfm.gsub_script_tag);

  pickup scriptlst (fun gs -> Otfm.gsub_script_tag gs = scripttag)
    (`Msg(str "GSUB does not contain Script tag '%s'" scripttag)) >>= fun script ->
  Otfm.gsub_langsys script >>= fun (langsys_DFLT, _) ->
  Otfm.gsub_feature langsys_DFLT >>= fun (_, featurelst) ->

  Format.printf "all GSUB Feature tags for '%s', 'DFLT':@ @[[%a]@]@," scripttag
    (Format.pp_print_list ~pp_sep Format.pp_print_string)
      (featurelst |> List.map Otfm.gsub_feature_tag);

  pickup featurelst (fun gf -> Otfm.gsub_feature_tag gf = type4tag)
    (`Msg(str "GSUB does not contain Feature tag '%s' for '%s', 'DFLT'" type4tag scripttag)) >>= fun feature_type4 ->
  Otfm.gsub feature_type4 ~lig:f_lig [] >>= fun type4ret ->

  Format.printf "finish '%s'@," type4tag;

  pickup featurelst (fun gf -> Otfm.gsub_feature_tag gf = type3tag)
    (`Msg(str "GSUB does not contain Feature tag '%s' for '%s', 'DFLT'" type3tag scripttag)) >>= fun feature_type3 ->

  Otfm.gsub feature_type3 ~single:f_single ~alt:f_alt [] >>= fun type3ret ->

  Format.printf "finish '%s'@," type3tag;
  Format.printf "@]@,";

  return (type3ret, type4ret)


let decode_gpos scripttag tag d =
  Otfm.gpos_script d >>= fun scriptlst ->

  Format.printf "@[<v2>@,";
  Format.printf "all GPOS Script tags:@ @[[%a]@]@,"
    (Format.pp_print_list ~pp_sep Format.pp_print_string)
      (scriptlst |> List.map Otfm.gpos_script_tag);

  pickup scriptlst (fun script -> Otfm.gpos_script_tag script = scripttag)
    (`Msg("GPOS does not contain Script tag 'latn'")) >>= fun script_latn ->
  Otfm.gpos_langsys script_latn >>= fun (langsys_DFLT, _) ->
  Otfm.gpos_feature langsys_DFLT >>= fun (_, featurelst) ->

  Format.printf "all GPOS Feature tags for '%s', 'DFLT':@ @[[%a]@]@," scripttag
    (Format.pp_print_list ~pp_sep Format.pp_print_string)
      (featurelst |> List.map Otfm.gpos_feature_tag);

  Format.printf "@]@,";

  pickup featurelst (fun feature -> Otfm.gpos_feature_tag feature = tag)
    (`Msg(str "GPOS does not contain Feature tag '%s' for '%s'" tag scripttag)) >>= fun feature_kern ->
  Otfm.gpos feature_kern ~single1:f_single1 ~single2:f_single2 ~pair1:f_pair1 ~pair2:f_pair2 ~markbase1:f_markbase1 [] >>= fun gposres ->
  return gposres


let main filename script type3tag type4tag gpostag =
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  Otfm.decoder (`String(src)) >>= function
  | Otfm.SingleDecoder(d) ->
      begin
        Format.printf "@[<v2>@,";
        Format.printf "finish initializing decoder@,";
        decode_gsub script type3tag type4tag d >>= fun (type3ret, type4ret) ->
        decode_gpos script gpostag d >>= fun gposret ->
        Format.printf "finish decoding@,";
        Format.printf "@]@,";
        return (type3ret, type4ret, gposret)
      end

  | Otfm.TrueTypeCollection(_) ->
      let () = print_endline "TTC file" in
      return ([], [], [])


let pp_ligature_set fmt (gidtail, gid) =
  Format.printf "@[[%a]@] ---> %d@," (Format.pp_print_list ~pp_sep Format.pp_print_int) gidtail gid


let () =
  let (filename, script, type3tag, type4tag, gpostag) =
    try (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4), Sys.argv.(5)) with
    | Invalid_argument(_) -> begin print_endline "illegal argument"; exit 1 end
  in
  match main filename script type3tag type4tag gpostag with
  | Error(#Otfm.error as e) ->
      Format.printf "@[<v2>error1@,";
      Format.printf "@[%a@]@]@," Otfm.pp_error e

  | Error(`Msg(msg)) ->
      Format.printf "@[<v2>error2@,";
      Format.printf "!!!! %s !!!!@]@," msg

  | Ok(gid_altset_assoc, gidfst_ligset_assoc, clsfst_pairposlst_assoc) ->
      begin
        Format.printf "@[<v2>@,";
        Format.printf "@[<v2>GSUB '%s':@," type3tag;
        gid_altset_assoc |> List.rev |> List.iter (fun (gid, gidlst) ->
          Format.printf "%d -> [@[%a@]]@," gid
            (Format.pp_print_list ~pp_sep Format.pp_print_int) gidlst;
        );
        Format.printf "@]@,";

        Format.printf "@[<v2>GSUB '%s':@," type4tag;
        gidfst_ligset_assoc |> List.rev |> List.iter (fun (gidfst, ligset) ->
          Format.printf "%d -> @[[%a]@]@," gidfst (Format.pp_print_list ~pp_sep pp_ligature_set) ligset
        );
        Format.printf "@]@,";

        Format.printf "@[<v2>GPOS '%s':@," gpostag;
        clsfst_pairposlst_assoc |> List.rev |> List.iter (function
        | Single1(gidlst, valrcd) ->
            Format.printf ("#Single1@,@[%a@]@,")
              (Format.pp_print_list ~pp_sep Format.pp_print_int) gidlst

        | Pair1(gidfst, pairposset) ->
            Format.printf "#Pair1 %d -> (length: %d)@," gidfst (List.length pairposset)

        | Pair2(clsfst, pairposset) ->
            Format.printf "#Pair2 %d -> (length: %d)@," clsfst (List.length pairposset)
        );
        Format.printf "@]@,";
        Format.printf "@]";
      end
