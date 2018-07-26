
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
