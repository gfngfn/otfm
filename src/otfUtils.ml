open OtfTypes

let debugfmt =
  let dev_null = if Sys.os_type = "Win32" then "NUL" else "/dev/null" in
  Format.formatter_of_out_channel (open_out dev_null)

let fmtgen  = debugfmt
let fmtGSUB = debugfmt
let fmtMATH = debugfmt
(*
let fmtCFF  = Format.std_formatter
*)

module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
end = struct

  type 'a t = 'a list

  let empty = []

  let extend acc x =
    x :: acc

  let append acc lst =
    List.rev_append lst acc

  let to_list acc =
    List.rev acc

end

(* Unsafe string byte manipulations.

   If you don't believe the author's invariants, replacing with safe
   versions makes everything safe in the module. He won't be
   upset. *)

let unsafe_chr = Char.unsafe_chr
let unsafe_byte s j = Char.code (String.unsafe_get s j)

(* Pretty printers *)

let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)


let ( >>= ) x f = match x with Ok(v) -> f v | Error(_) as e -> e
let return x                 = Ok(x)
let err e                    = Error(e)


let confirm b e =
  if not b then err e else return ()
