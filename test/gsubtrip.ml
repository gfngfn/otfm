
open OtfTypes
open OtfUtils

open TestUtil
(* overwrites `>>=`, `return`, etc. *)


type position =
  | Single1 of Otfm.glyph_id list * Otfm.value_record
  | Pair1 of Otfm.glyph_id * (Otfm.glyph_id * Otfm.value_record * Otfm.value_record) list
  | Pair2 of Otfm.class_value * (Otfm.class_value * Otfm.value_record * Otfm.value_record) list
  | MarkBase1 of int * (Otfm.glyph_id * Otfm.mark_record) list * (Otfm.glyph_id * Otfm.base_record) list
  | MarkMark1 of int * (Otfm.glyph_id * Otfm.mark_record) list * (Otfm.glyph_id * Otfm.mark2_record) list


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

let f_markbase1 classCount acc markassoc baseassoc =
  MarkBase1(classCount, markassoc, baseassoc) :: acc


let f_markmark1 classCount acc markassoc mark2assoc =
  MarkMark1(classCount, markassoc, mark2assoc) :: acc


let pp_sep fmt () =
  Format.fprintf fmt ",@ "


let decode_gsub scripttag type3tag type4tag d =
  Otfm.gsub_script d >>= function
  | None ->
      Format.printf "No GSUB@,";
      return ([], [])

  | Some(scriptlst) ->
      Format.printf "@[<v2>@,";
      Format.printf "all GSUB Script tags:@ @[[%a]@]@,"
        (Format.pp_print_list ~pp_sep Format.pp_print_string)
          (scriptlst |> List.map Otfm.gsub_script_tag);

      pickup scriptlst (fun gs -> Otfm.gsub_script_tag gs = scripttag)
        (`Msg(str "GSUB does not contain Script tag '%s'" scripttag)) >>= fun script ->
      Otfm.gsub_langsys script >>= fun langsys_res ->
      let langsys =
        match langsys_res with
        | (Some(langsys_DFLT), _)    -> langsys_DFLT
        | (None, langsys_first :: _) -> langsys_first
        | (None, [])                 -> failwith "no langsys"
      in
      Otfm.gsub_feature langsys >>= fun (_, featurelst) ->

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
  Otfm.gpos_script d >>= function
  | None ->
      Format.printf "no GPOS@,";
      return []

  | Some(scriptlst) ->
      Format.printf "@[<v2>@,";
      Format.printf "all GPOS Script tags:@ @[[%a]@]@,"
        (Format.pp_print_list ~pp_sep Format.pp_print_string)
          (scriptlst |> List.map Otfm.gpos_script_tag);

      pickup scriptlst (fun script -> Otfm.gpos_script_tag script = scripttag)
        (`Msg("GPOS does not contain Script tag '" ^ scripttag ^ "'")) >>= fun script_latn ->
      Otfm.gpos_langsys script_latn >>= fun langsys_res ->
      let langsys =
        match langsys_res with
        | (Some(langsys_DFLT), _)    -> langsys_DFLT
        | (None, langsys_first :: _) -> langsys_first
        | (None, [])                 -> failwith "no langsys"
      in
      Otfm.gpos_feature langsys >>= fun (_, featurelst) ->

      Format.printf "all GPOS Feature tags for '%s', 'DFLT':@ @[[%a]@]@," scripttag
        (Format.pp_print_list ~pp_sep Format.pp_print_string)
          (featurelst |> List.map Otfm.gpos_feature_tag);

      Format.printf "@]@,";

      pickup featurelst (fun feature -> Otfm.gpos_feature_tag feature = tag)
        (`Msg(str "GPOS does not contain Feature tag '%s' for '%s'" tag scripttag)) >>= fun feature_kern ->
      Otfm.gpos feature_kern ~single1:f_single1 ~single2:f_single2 ~pair1:f_pair1 ~pair2:f_pair2 ~markbase1:f_markbase1 ~markmark1:f_markmark1 [] >>= fun gposres ->
      return gposres


let main filename script type3tag type4tag gpostag =
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  OtfDecBasic.decoder (`String(src)) >>= function
  | SingleDecoder(d) ->
      begin
        Format.printf "@[<v2>@,";
        Format.printf "finish initializing decoder@,";
        decode_gsub script type3tag type4tag d >>= fun (type3ret, type4ret) ->
        decode_gpos script gpostag d >>= fun gposret ->
        Format.printf "finish decoding@,";
        Format.printf "@]@,";
        return (type3ret, type4ret, gposret)
      end

  | TrueTypeCollection(_) ->
      let () = print_endline "TTC file" in
      return ([], [], [])


let pp_ligature_set fmt (gidtail, gid) =
  Format.fprintf fmt "@[[%a]@] ---> %d@," (Format.pp_print_list ~pp_sep Format.pp_print_int) gidtail gid


let pp_anchor fmt (dux, duy, _) =
  Format.fprintf fmt "(%d, %d)" dux duy


let pp_anchor_opt fmt = function
  | Some(dux, duy, _) -> Format.fprintf fmt "(%d, %d)" dux duy
  | None              -> Format.fprintf fmt "NULL"


let pp_mark_record fmt (gid, (markcls, anch)) =
  Format.fprintf fmt "%d --> %d %a" gid markcls pp_anchor anch


let pp_base_record fmt (gid, ancharr) =
  Format.fprintf fmt "%d --> {%a}" gid (Format.pp_print_list ~pp_sep pp_anchor_opt) (Array.to_list ancharr)


let pp_mark2_record fmt (gid, ancharr) =
  Format.fprintf fmt "%d --> {%a}" gid (Format.pp_print_list ~pp_sep pp_anchor) (Array.to_list ancharr)


let print_markbase s clscnt markassoc dataassoc =
  Format.printf "#MarkToData (classCount = %d)@,@[<v2>@," clscnt;
  Format.printf "mark: @[[%a]@]@," (Format.pp_print_list ~pp_sep pp_mark_record) markassoc;
  Format.printf "%s: @[[%a]@]" s (Format.pp_print_list ~pp_sep pp_base_record) dataassoc;
  Format.printf "@]@,"


let print_markmark s clscnt markassoc dataassoc =
  Format.printf "#MarkToData (classCount = %d)@,@[<v2>@," clscnt;
  Format.printf "mark: @[[%a]@]@," (Format.pp_print_list ~pp_sep pp_mark_record) markassoc;
  Format.printf "%s: @[[%a]@]" s (Format.pp_print_list ~pp_sep pp_mark2_record) dataassoc;
  Format.printf "@]@,"


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

        | MarkBase1(clscnt, markassoc, baseassoc) ->
            print_markbase "base" clscnt markassoc baseassoc

        | MarkMark1(clscnt, markassoc, mark2assoc) ->
            print_markmark "mark2" clscnt markassoc mark2assoc
        );
        Format.printf "@]@,";
        Format.printf "@]";
      end
