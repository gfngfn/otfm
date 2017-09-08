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

let main () =
  let ( >>= ) x f = match x with Ok(v) -> f v | Error(_) as e -> e in
  let filename = try Sys.argv.(1) with Invalid_argument(_) -> begin print_endline "illegal argument"; exit 1 end in
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  let d = Otfm.decoder (`String(src)) in
  let () = print_endline "finish initializing decoder" in
  Otfm.gsub d "latn" None >>= fun subtables ->
  Ok()

let () =
  match main () with
  | Ok()     -> print_endline "ok."
  | Error(e) -> Format.eprintf "@[%a@]@." Otfm.pp_error e
