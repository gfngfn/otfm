open OtfTypes

type t = WideInt.t

(* -- OpenType version tags -- *)

let v_wOFF = !%% 0x774F4646L
let v_OTTO = !%% 0x4F54544FL
let v_ttcf = !%% 0x74746366L
let v_true = !%% 0x74727565L (* -- may happen in the wild. -- *)
let v_1_0  = !%% 0x00010000L

(* -- Required common tables tags -- *)

let cmap = !%% 0x636D6170L
let head = !%% 0x68656164L
let hhea = !%% 0x68686561L
let hmtx = !%% 0x686D7478L
let maxp = !%% 0x6D617870L
let name = !%% 0x6E616D65L
let os2  = !%% 0x4F532F32L
let post = !%% 0x706F7374L

let t_common = [ cmap; head; hhea; hmtx; maxp; name; os2; post ]

(* -- TTF font table tags -- *)

let cvt  = !%% 0x63767420L
let fpgm = !%% 0x6670676DL
let glyf = !%% 0x676C7966L
let loca = !%% 0x6C6F6361L
let prep = !%% 0x70726570L

(* -- CFF font table tags -- *)

let cff  = !%% 0x43464620L
let vorg = !%% 0x564F5247L

(* -- Bitmap glyph tables -- *)

let ebdt = !%% 0x45424454L
let eblc = !%% 0x45424C43L
let ebsc = !%% 0x45425343L

(* -- Optional tables -- *)

let dsig = !%% 0x44534947L
let gasp = !%% 0x67617370L
let hdmx = !%% 0x68646D78L
let kern = !%% 0x6B65726EL
let ltsh = !%% 0x4C545348L
let pclt = !%% 0x50434C54L
let vdmx = !%% 0x56444D58L
let vhea = !%% 0x76686561L
let vmtx = !%% 0x766D7478L

(* -- Advanced Open Type font layout tables -- *)

let base = !%% 0x42415345L
let gdef = !%% 0x47444546L
let gpos = !%% 0x47504F53L
let gsub = !%% 0x47535542L
let jstf = !%% 0x4A535446L
let math = !%% 0x4d415448L

(* -- Functions -- *)

let of_bytes s =
  let open WideInt in
    if String.length s <> 4 then
      None
    else
      let b0 = of_byte (String.get s 0) in
      let b1 = of_byte (String.get s 1) in
      let b2 = of_byte (String.get s 2) in
      let b3 = of_byte (String.get s 3) in
      Some((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)


let to_bytes t =
  let (c0, c1, c2, c3) = cut_uint32_unsafe t in
  Printf.sprintf "%c%c%c%c" c0 c1 c2 c3


let to_wide_int x = x
let of_wide_int x = x

let compare = WideInt.compare
let pp ppf t = Format.fprintf ppf "'%s'" (to_bytes t)
