
open OtfTypes
open OtfUtils
open OtfDecBasic

type cff_decoder

val cff_common : cff_decoder -> common_decoder

val make_initial_cff : common_decoder -> cff_decoder
