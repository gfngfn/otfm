
open OtfTypes
open OtfUtils
open OtfDecBasic
module Tag = OtfTag


type cff_specific = unit (* FIXME *)

type cff_decoder = {
  cff_common   : common_decoder;
  cff_specific : cff_specific;
}


let cff_common (dcff : cff_decoder) : common_decoder =
  dcff.cff_common


let make_initial_cff (cd : common_decoder) : cff_decoder =
  {
    cff_common = cd;
    cff_specific = (); (* FIXME; shoule be a record of TTF-specific data *)
  }
