
open OtfTypes
open OtfUtils

type error = OtfError.t

type 'a ok = ('a, error) result

type tag = OtfTag.t

type source = [ `String of string ]
(** The type for input sources. *)

type common_decoder

val make_initial_decoder : source -> common_decoder

val decoder_source : common_decoder -> source
(** [decoder_source d] is [d]'s input source. *)

val copy_and_initialize_decoder : common_decoder -> common_decoder

val e_version : common_decoder -> WideInt.t -> error

val err_version : common_decoder -> WideInt.t -> 'a ok

val err_loca_format : common_decoder -> int -> 'a ok

val err_composite_format : common_decoder -> int -> 'a ok

val err_coverage_length : common_decoder -> 'a ok

val cur_pos : common_decoder -> int
(** [cur_pos d] returns [d]'s current position (in byte offset). *)

val seek_pos : int -> common_decoder -> unit ok
(** [seek_pos pos d] moves [d]'s current position to [pos]. *)

val seek_table : tag -> common_decoder -> (int option) ok
(** moves [d]'s current position at the beginning of the table of the given tag and returns the length of the table if exists, or returns [None] otherwise. *)

val seek_required_table : tag -> common_decoder -> unit ok
(** moves [d]'s current position at the beginning of the table of the given tag. *)

val d_skip : int -> common_decoder -> unit ok
(** [d_skip len d] advances [d]'s position by [len] byte(s). *)

val d_bytes : int -> common_decoder -> string ok

val d_uint8 : common_decoder -> int ok
(** [d_uint8 d] decodes an uint8 value and move forward by 1 byte. *)

val d_int8 : common_decoder -> int ok
(** [d_int8 d] decodes an int8 value and move forward by 1 byte. *)

val d_uint16 : common_decoder -> int ok
(** [d_uint16 d] decodes an uint16 value and move forward by 2 bytes. *)

val d_int16 : common_decoder -> int ok
(** [d_int16 d] decodes an int16 value and move forward by 2 bytes. *)

val d_uint24 : common_decoder -> int ok

val d_uint32 : common_decoder -> WideInt.t ok
(** [d_int32 d] decodes an uint32 value and move forward by 4 bytes. *)

val d_uint32_int : common_decoder -> int ok
(** [d_int32_int d] decodes an uint32 value and move forward by 4 bytes. *)

val d_int32 : common_decoder -> WideInt.t ok
(** [d_int32 d] decodes an int32 value and move forward by 4 bytes. *)

val d_time : common_decoder -> WideInt.t ok

val d_f2dot14 : common_decoder -> float ok

val d_utf_16be : int -> common_decoder -> string ok

val d_device_table : common_decoder -> device_table ok

val d_repeat : int -> (common_decoder -> 'a ok) -> common_decoder -> ('a list) ok
(** [d_repeat n df d] decodes [d] by using [df] consecutively [n] time(s). *)

val d_list : (common_decoder -> 'a ok) -> common_decoder -> ('a list) ok
(** [d_list df d] decodes list-storing, variable-length records. *)

val d_list_filtered : (common_decoder -> 'a ok) -> (int -> bool) -> common_decoder -> ('a list) ok

val d_list_access : (common_decoder -> 'a ok) -> int -> common_decoder -> ('a option) ok

val d_offset_list : int -> common_decoder -> (int list) ok

val d_offset : int -> common_decoder -> int ok
(** [d_offset origin d] decodes a 2-byte offset relative to the absolute offset [origin]. *)

val d_offset_opt : int -> common_decoder -> (int option) ok
(** same as [d_offset] except that it treats [0] as [None]. *)

val d_fetch : int -> (common_decoder -> 'a ok) -> common_decoder -> 'a ok

val d_fetch_opt : int -> (common_decoder -> 'a ok) -> common_decoder -> ('a option) ok

val d_fetch_list : int -> (common_decoder -> ('a list) ok) -> common_decoder -> ('a list) ok

val d_fetch_long : int -> (common_decoder -> 'a ok) -> common_decoder -> (int * 'a) ok

val init_decoder : common_decoder -> unit ok
(** initializes a decoder. *)

val d_ttc_header_offset_list : common_decoder -> (int list) ok

val table_list : common_decoder -> (tag list) ok
(** [table_list t] is the list of tables of the font decoded by [d]. *)

val table_mem : common_decoder -> tag -> bool ok
(** [table_mem d t] is [true] if table [t] is in the font decoded by [d]. *)

val table_raw : common_decoder -> tag -> (string option) ok
(** [table_raw d t] is the (unpadded) data of the table [t] as a
    string if the table [t] exists. *)

(** {2:convenience Convenience decodes}

    These functions lookup data in the right table. *)

val glyph_count : common_decoder -> int ok
(** [glyph_count d] is the number of glyphs in the font (bounded by [65535]). *)

val postscript_name : common_decoder -> (string option) ok
(** [poscript_name d] is the PostScript name of [d]. Looks up and validates
    as mandated by the OTF standard, don't rely on {!name} if you really
    need this information. *)
