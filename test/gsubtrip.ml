open Result

let str = Format.sprintf

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
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
    | Exit -> close ic; Ok (Buffer.contents b)
    | Failure _ -> close ic; Error (`Msg (str "%s: input file too large" inf))
    | Sys_error e -> close ic; (Error (`Msg e));
  with
  | Sys_error e -> (Error (`Msg e))


type error = [ Otfm.error | `Msg of string ]

let ( >>= ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e :> error)

let return v = Ok(v)

let err e = Error(e)

let pickup lst predicate e =
  match lst |> List.filter predicate with
  | []     -> err e
  | v :: _ -> return v


type pair_position =
  | Pair1 of Otfm.glyph_id * (Otfm.glyph_id * Otfm.value_record * Otfm.value_record) list
  | Pair2 of Otfm.class_value * (Otfm.class_value * Otfm.value_record * Otfm.value_record) list


let f_lig lst (gidfst, liginfolst) =
  (gidfst, liginfolst) :: lst

let f_pair1 lst (gidfst, pairinfolst) =
  Pair1(gidfst, pairinfolst) :: lst

(*
let f_pair2 clsdeflst1 clsdeflst2 lst (clsval, pairinfolst) =
  Pair2(clsval, pairinfolst) :: lst
*)
let f_pair2 _ _ lst _ = lst


let decode_gsub d =
  Otfm.gsub_script d >>= fun scriptlst ->
  pickup scriptlst (fun script -> Otfm.gsub_script_tag script = "latn")
    (`Msg("GSUB does not contain Script tag 'latn'")) >>= fun script_latn ->
  Otfm.gsub_langsys script_latn >>= fun (langsys_DFLT, _) ->
  Otfm.gsub_feature langsys_DFLT >>= fun (_, featurelst) ->
  pickup featurelst (fun feature -> Otfm.gsub_feature_tag feature = "liga")
    (`Msg("GSUB does not contain Feature tag 'liga' for 'latn'")) >>= fun feature_liga ->
  Otfm.gsub feature_liga f_lig [] >>= fun gsubres ->
  return gsubres


let decode_gpos d =
  Otfm.gpos_script d >>= fun scriptlst ->
  pickup scriptlst (fun script -> Otfm.gpos_script_tag script = "latn")
    (`Msg("GPOS does not contain Script tag 'latn'")) >>= fun script_latn ->
  Otfm.gpos_langsys script_latn >>= fun (langsys_DFLT, _) ->
  Otfm.gpos_feature langsys_DFLT >>= fun (_, featurelst) ->
  pickup featurelst (fun feature -> Otfm.gpos_feature_tag feature = "kern")
    (`Msg("GPOS does not contain Feature tag 'kern' for 'latn'")) >>= fun feature_kern ->
  Otfm.gpos feature_kern f_pair1 f_pair2 [] >>= fun gposres ->
  return gposres


let main () =
  let filename =
    try Sys.argv.(1) with
    | Invalid_argument(_) -> begin print_endline "illegal argument"; exit 1 end
  in
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  Otfm.decoder (`String(src)) >>= function
  | Otfm.SingleDecoder(d) ->
      let () = print_endline "finish initializing decoder" in
      begin
        decode_gsub d >>= fun gsubres ->
        decode_gpos d >>= fun gposres ->
        return (gsubres, gposres)
      end

  | Otfm.TrueTypeCollection(_) ->
      let () = print_endline "TTC file" in
      return ([], [])


let () =
  match main () with
  | Error(#Otfm.error as e) -> Format.eprintf "@[%a@]@." Otfm.pp_error e
  | Error(`Msg(msg))        -> Format.eprintf "%s" msg
  | Ok(gidfst_ligset_assoc, clsfst_pairposlst_assoc) ->
      begin
        print_endline "GSUB:";
        gidfst_ligset_assoc |> List.rev |> List.iter (fun (gidfst, ligset) ->
          print_string ((string_of_int gidfst) ^ " -> [");
          ligset |> List.iter (fun (gidtail, gidlig) ->
            gidtail |> List.iter (fun gid -> print_string (" " ^ (string_of_int gid)));
            print_string (" ----> " ^ (string_of_int gidlig) ^ "; ");
          );
          print_endline "]";
        );
        print_endline "GPOS:";
        clsfst_pairposlst_assoc |> List.rev |> List.iter (function
        | Pair1(gidfst, pairposset) ->
            print_endline ("[1] " ^ (string_of_int gidfst) ^ " -> (length: " ^
              (string_of_int (List.length pairposset)) ^ ")")
        | Pair2(clsfst, pairposset) ->
            print_endline ("[2] " ^ (string_of_int clsfst) ^ " -> (length: " ^
              (string_of_int (List.length pairposset)) ^ ")")
        );
      end
