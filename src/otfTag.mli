open OtfTypes

(** {1 Tags} *)

(** Tags.

    OpenType tags are four bytes identifiers. *)

type t
(** The type for OpenType tags. *)

val v_wOFF : t
val v_OTTO : t
val v_ttcf : t
val v_true : t
val v_1_0 : t

(** {1 Table tags} *)

(** {2:req Required tables} *)

val cmap : t
(** The {{:https://www.microsoft.com/typography/otspec/cmap.htm}cmap} table. *)

val head : t
(** The {{:https://www.microsoft.com/typography/otspec/head.htm}head} table. *)

val hhea : t
(** The {{:https://www.microsoft.com/typography/otspec/hhea.htm}hhea} table. *)

val hmtx : t
(** The {{:https://www.microsoft.com/typography/otspec/hmtx.htm}hmtx} table. *)

val maxp : t
(** The {{:https://www.microsoft.com/typography/otspec/maxp.htm}maxp} table. *)

val name : t
(** The {{:https://www.microsoft.com/typography/otspec/name.htm}name} table. *)

val os2 : t
(** The {{:https://www.microsoft.com/typography/otspec/os2.htm}os2} table. *)

val post : t
(** The {{:https://www.microsoft.com/typography/otspec/post.htm}post} table. *)


(** {2 TTF font tables} *)

val cvt  : t
(** The {{:https://www.microsoft.com/typography/otspec/cvt.htm}cvt} table. *)

val fpgm : t
(** The {{:https://www.microsoft.com/typography/otspec/fpgm.htm}fpgm} table. *)

val glyf : t
(** The {{:https://www.microsoft.com/typography/otspec/glyf.htm}glyf} table. *)

val loca : t
(** The {{:https://www.microsoft.com/typography/otspec/loca.htm}loca} table. *)

val prep : t
(** The {{:https://www.microsoft.com/typography/otspec/prep.htm}prep} table. *)


(** {2 CFF font tables} *)

val cff  : t
(** The {{:https://www.microsoft.com/typography/otspec/cff.htm}CFF} table. *)
val vorg : t
(** The {{:https://www.microsoft.com/typography/otspec/vorg.htm}VORG} table. *)

(** {2 Bitmap glyph tables} *)

val ebdt : t
(** The {{:https://www.microsoft.com/typography/otspec/ebdt.htm}EBDT} table. *)

val eblc : t
(** The {{:https://www.microsoft.com/typography/otspec/eblc.htm}EBLC} table. *)

val ebsc : t
(** The {{:https://www.microsoft.com/typography/otspec/ebsc.htm}EBSC} table. *)


(** {2 Optional tables} *)

val dsig : t
(** The {{:https://www.microsoft.com/typography/otspec/dsig.htm}DSIG} table. *)

val gasp : t
(** The {{:https://www.microsoft.com/typography/otspec/gasp.htm}gasp} table. *)

val hdmx : t
(** The {{:https://www.microsoft.com/typography/otspec/hdmx.htm}hdmx} table. *)

val kern : t
(** The {{:https://www.microsoft.com/typography/otspec/kern.htm}kern} table. *)

val ltsh : t
(** The {{:https://www.microsoft.com/typography/otspec/ltsh.htm}LTSH} table. *)

val pclt : t
(** The {{:https://www.microsoft.com/typography/otspec/pclt.htm}PCLT} table. *)

val vdmx : t
(** The {{:https://www.microsoft.com/typography/otspec/vdmx.htm}VDMX} table. *)

val vhea : t
(** The {{:https://www.microsoft.com/typography/otspec/vhea.htm}vhea} table. *)

val vmtx : t
(** The {{:https://www.microsoft.com/typography/otspec/vmtx.htm}vmtx} table. *)


(** {2 Advanced typographic tables} *)

val base : t
(** The {{:https://www.microsoft.com/typography/otspec/base.htm}BASE} table. *)

val gdef : t
(** The {{:https://www.microsoft.com/typography/otspec/gdef.htm}GDEF} table. *)

val gpos : t
(** The {{:https://www.microsoft.com/typography/otspec/gpos.htm}GPOS} table. *)

val gsub : t
(** The {{:https://www.microsoft.com/typography/otspec/gsub.htm}GSUB} table. *)

val jstf : t
(** The {{:https://www.microsoft.com/typography/otspec/jstf.htm}JSTF} table. *)

val math : t
(** The {{:https://www.microsoft.com/typography/otspec/math.htm}MATH} table. *)


(** {1 Functions} *)

val of_bytes : string -> t option
(** [of_bytes s] is a tag corresponding to [s].
    @raise Invalid_argument if [s] is not four byte long. *)

val to_bytes : t -> string
(** [to_string t] is the tag as a four byte long string. *)

val to_wide_int : t -> WideInt.t
(** [to_wide_int t] is the tag as a wide-range integer. *)

val of_wide_int : WideInt.t -> t
(** [of_wide_int wi] is the tag from wide-range integer. *)

val compare : t -> t -> int
(** [compare t t'] is [Pervasives.compare t t'] *)

val pp : Format.formatter -> t -> unit
(** [pp t] prints a textual representation of [t] on [ppf]. *)
