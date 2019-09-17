open OtfTypes

val debugfmt : Format.formatter

val fmtgen : Format.formatter

val fmtGSUB : Format.formatter

val fmtMATH : Format.formatter

module Alist :
  sig
    type 'a t
    val empty : 'a t
    val extend : 'a t -> 'a -> 'a t
    val append : 'a t -> 'a list -> 'a t
    val to_list : 'a t -> 'a list
  end

val unsafe_chr : int -> char

val unsafe_byte : string -> int -> int

val pp_list :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list -> unit

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

val return : 'a -> ('a, 'b) result

val err : 'a -> ('b, 'a) result

val confirm : bool -> OtfError.t -> (unit, OtfError.t) result

val is_in_range : int -> int -> int -> bool
