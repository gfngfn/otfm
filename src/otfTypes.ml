
type loc_format =
  | ShortLocFormat
  | LongLocFormat

type flavour = TTF_true | TTF_OT | CFF
(** The type for OpenType flavours. *)

type device_table = int * int * int * int

type byte = char

module WideInt : sig
  type t
  val ( lsl ) : t -> int -> t
  val ( lsr ) : t -> int -> t
  val ( lor ) : t -> t -> t
  val ( land ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_byte : byte -> t
  val to_byte : t -> byte
  val is_in_int32 : t -> bool
  val is_in_uint32 : t -> bool
  val is_in_int64 : t -> bool
  val is_neg : t -> bool
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Int64.t
  let ( lsl ) = Int64.shift_left
  let ( lsr ) = Int64.shift_right
  let ( lor ) = Int64.logor
  let ( land ) = Int64.logand
  let ( mod ) = Int64.rem
  let add = Int64.add
  let sub = Int64.sub
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let of_int64 iw = iw
  let to_int64 iw = iw
  let of_byte ch = Int64.of_int (Char.code ch)
  let to_byte iw = Char.chr (Int64.to_int iw)  (* -- may raise 'Invalid_argument' -- *)
  let is_in_uint32 iw = Int64.zero <= iw && iw < 0x100000000L
  let is_in_int32 iw = -0x80000000L <= iw && iw < 0x80000000L
  let is_in_int64 iw = Int64.min_int <= iw && iw <= Int64.max_int
  let is_neg iw = iw < Int64.zero
  let pp fmt iw = Format.fprintf fmt "%LX" iw
end

type wint = WideInt.t

let ( +% ) = WideInt.add
let ( -% ) = WideInt.sub
let ( !% ) = WideInt.of_int
let ( !%% ) = WideInt.of_int64


let cut_uint32_unsafe (ui : wint) : byte * byte * byte * byte =
  let open WideInt in
    let b0 = ui lsr 24 in
    let r0 = ui -% (b0 lsl 24) in
    let b1 = r0 lsr 16 in
    let r1 = r0 -% (b1 lsl 16) in
    let b2 = r1 lsr 8 in
    let b3 = r1 -% (b2 lsl 8) in
    (to_byte b0, to_byte b1, to_byte b2, to_byte b3)

type cp = int
(** The type for Unicode
    {{:http://unicode.org/glossary/#code_point}code points}, ranges
    from [0x0000] to [0x10_FFFF]. Any code point returned by
    [Otfm] is guaranteed to be in the range. *)

type cp_range = cp * cp
(** The type for Unicode code point ranges. Any range [(u0, u1)]
    returned by [Otfm] has [u0 <= u1]. *)