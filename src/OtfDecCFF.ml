
open OtfTypes
open OtfCFFTypes
open OtfUtils
open OtfDecBasic
module Tag = OtfTag


type cff_specific = unit (* FIXME *)

type cff_decoder = {
  cff_common   : common_decoder;
  cff_specific : cff_specific;
}

type charstring_info = cff_decoder * subroutine_index * private_info * int

type cff_info =
  {
    cff_first           : cff_first;
    font_name           : string;
    is_fixed_pitch      : bool;
    italic_angle        : int;
    underline_position  : int;
    underline_thickness : int;
    paint_type          : int;
    (* font_matrix : float * float * float * float; *)
    font_bbox           : int * int * int * int;
    stroke_width        : int;
    cid_info            : cff_cid_info option;
    number_of_glyphs    : int;
    charstring_info     : charstring_info;
  }
    (* -- the type for data basically corresponding to Top DICTs [CFF p.15, Table 9] -- *)


let cff_common (dcff : cff_decoder) : common_decoder =
  dcff.cff_common


let make_initial_cff (cd : common_decoder) : cff_decoder =
  {
    cff_common = cd;
    cff_specific = (); (* FIXME; shoule be a record of TTF-specific data *)
  }


let d_offsize d : offsize ok =
  d_uint8 d >>= fun i ->
    match i with
    | 1 -> return OffSize1
    | 2 -> return OffSize2
    | 3 -> return OffSize3
    | 4 -> return OffSize4
    | n -> err (`Invalid_cff_not_an_offsize(n))


let pp_offsize fmt = function
  | OffSize1 -> Format.fprintf fmt "OffSize1"
  | OffSize2 -> Format.fprintf fmt "OffSize2"
  | OffSize3 -> Format.fprintf fmt "OffSize3"
  | OffSize4 -> Format.fprintf fmt "OffSize4"


let d_cff_offset ofsz d : wint ok =
  match ofsz with
  | OffSize1 -> d_uint8  d >>= fun i -> return (!% i)
  | OffSize2 -> d_uint16 d >>= fun i -> return (!% i)
  | OffSize3 -> d_uint24 d >>= fun i -> return (!% i)
  | OffSize4 -> d_uint32 d


let d_cff_offset_singleton ofsz dl d =
  d_cff_offset ofsz d                                        >>= fun offset1 ->
  confirm (offset1 = !% 1) `Invalid_cff_invalid_first_offset >>= fun () ->
  d_cff_offset ofsz d                                        >>= fun offset2 ->
  dl (WideInt.to_int (offset2 -% offset1)) d


let d_index_singleton dl d =
  d_uint16 d                                       >>= fun count ->
  confirm (count = 1) `Invalid_cff_not_a_singleton >>= fun () ->
  d_offsize d                                      >>= fun offSize ->
  d_cff_offset_singleton offSize dl d              >>= fun v ->
  return v


let d_cff_length_list ofsz count d =
  let rec aux offsetprev acc i =
    if i >= count then
      return (Alist.to_list acc)
    else
      d_cff_offset ofsz d >>= fun offset ->
      let len = WideInt.to_int (offset -% offsetprev) in
      aux offset (Alist.extend acc len) (i + 1)
  in
  d_cff_offset ofsz d                                        >>= fun offset1 ->
  confirm (offset1 = !% 1) `Invalid_cff_invalid_first_offset >>= fun () ->
  aux (!% 1) Alist.empty 0


let d_index (type a) (dummy : a) (cdl : int -> common_decoder -> a ok) (cd : common_decoder) : (a array) ok =
  let rec loop_data arr i = function
    | [] ->
        return ()

    | len :: lentail ->
        cdl len cd >>= fun v ->
        arr.(i) <- v;
        loop_data arr (i + 1) lentail
  in
  d_uint16 cd >>= fun count ->
(*
  Format.fprintf fmtCFF "INDEX count: %d@," count;  (* for debug *)
*)
  if count = 0 then
    return [| |]
  else
    let arr = Array.make count dummy in
    d_offsize cd                       >>= fun offSize ->
(*
    Format.fprintf fmtCFF "INDEX offSize: %a@," pp_offsize offSize;  (* for debug *)
*)
    d_cff_length_list offSize count cd >>= fun lenlst ->
    loop_data arr 0 lenlst >>= fun () ->
    return arr


let d_charstring_data (len : int) (cd : common_decoder) : charstring_data ok =
  let offset = cur_pos cd in
  seek_pos (offset + len) cd >>= fun () ->
  return (CharStringData(offset, len))


let d_cff_real d =
(*
  Format.fprintf fmtCFF "d_cff_real@,";  (* for debug *)
*)
  let to_float lst =
    float_of_string (String.concat "" lst)
  in
  let nibble = function
    | d  when d |> is_in_range 0 9 -> return (string_of_int d)
    | 10                           -> return "."
    | 11                           -> return "e"
    | 12                           -> return "e-"
    | 13                           -> err `Invalid_cff_not_an_element
    | 14                           -> return "-"
    | 15                           -> return ""
    | _                            -> err `Invalid_cff_not_an_element
  in
  let rec aux step acc =
    d_uint8 d >>= fun raw ->
    let q1 = raw / 16 in
    let q2 = raw mod 16 in
    if q1 = 15 then
      if q2 = 15 then
        return (step + 1, to_float (Alist.to_list acc))
      else
        err `Invalid_cff_not_an_element
    else
      if q2 = 15 then
        nibble q1 >>= fun nb1 ->
        return (step + 1, to_float (Alist.to_list (Alist.extend acc nb1)))
      else
        nibble q2 >>= fun nb2 ->
        nibble q1 >>= fun nb1 ->
        aux (step + 1) (Alist.extend (Alist.extend acc nb1) nb2)
  in
  aux 0 Alist.empty


let d_index_access (type a) (cdl : int -> common_decoder -> a ok) (iaccess : int) (cd : common_decoder) : (a option) ok =
  d_uint16 cd >>= fun count ->
(*
  Format.fprintf fmtCFF "count = %d@," count;  (* for debug *)
*)
  if iaccess < 0 || count <= iaccess then
    return None
  else
    d_offsize cd >>= fun offSize ->
    let ofszint =
      match offSize with
      | OffSize1 -> 1
      | OffSize2 -> 2
      | OffSize3 -> 3
      | OffSize4 -> 4
    in
(*
    Format.fprintf fmtCFF "OffSize = %a@," pp_offsize offSize;  (* for debug *)
*)
    let offset_origin = (cur_pos cd) + (count + 1) * ofszint - 1 in
    d_skip (iaccess * ofszint) cd >>= fun () ->
    d_cff_offset offSize cd >>= fun reloffset_access ->
    d_cff_offset offSize cd >>= fun reloffset_next ->
    let offset_access = offset_origin + (WideInt.to_int reloffset_access) in
    let data_length = WideInt.to_int (reloffset_next -% reloffset_access) in
    seek_pos offset_access cd >>= fun () ->
    cdl data_length cd >>= fun data ->
    return (Some(data))


let d_twoscompl2 d =
  d_uint8 d >>= fun b1 ->
  d_uint8 d >>= fun b2 ->
  let iraw = (b1 lsl 8) lor b2 in
  let ret =
    if iraw >= (1 lsl 15) then
      iraw - (1 lsl 16)
    else
      iraw
  in
  return ret


let d_twoscompl4 d =
  d_uint8 d >>= fun b1 ->
  d_uint8 d >>= fun b2 ->
  d_uint8 d >>= fun b3 ->
  d_uint8 d >>= fun b4 ->
  let iraw = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
  let ret =
    if iraw >= (1 lsl 31) then
      iraw - (1 lsl 32)
    else
      iraw
  in
  return ret


let d_dict_element d : (int * dict_element) ok =
  d_uint8 d >>= function
    | k0  when k0 |> is_in_range 0 11 ->
        return (1, Key(ShortKey(k0)))

    | 12 ->
        d_uint8 d >>= fun k1 ->
        return (2, Key(LongKey(k1)))

    | k0  when k0 |> is_in_range 13 21 ->
        return (1, Key(ShortKey(k0)))

    | 28 ->
        d_twoscompl2 d >>= fun ret ->
        return (3, Value(Integer(ret)))

    | 29 ->
        d_twoscompl4 d >>= fun ret ->
        return (5, Value(Integer(ret)))

    | 30 ->
        d_cff_real d >>= fun (step, real) ->
        return (1 + step, Value(Real(real)))

    | b0  when b0 |> is_in_range 32 246 ->
        return (1, Value(Integer(b0 - 139)))

    | b0  when b0 |> is_in_range 247 250 ->
        d_uint8 d >>= fun b1 ->
        return (2, Value(Integer((b0 - 247) * 256 + b1 + 108)))

    | b0  when b0 |> is_in_range 251 254 ->
        d_uint8 d >>= fun b1 ->
        return (2, Value(Integer(-(b0 - 251) * 256 - b1 - 108)))

    | _ ->
        err `Invalid_cff_not_an_element


let pp_element ppf = function
  | Value(Integer(i)) -> Format.fprintf ppf "Integer(%d)" i
  | Value(Real(r))    -> Format.fprintf ppf "Real(%f)" r
  | Key(LongKey(x))   -> Format.fprintf ppf "LongKey(%d)" x
  | Key(ShortKey(x))  -> Format.fprintf ppf "ShortKey(%d)" x


(* -- `d_dict_keyval d` returns `(steps, vals, key)` where:

   * `steps`: how many steps the decoder ran to read the single key-value pair,
   * `vals`: the list of operands, and
   * `key`: the operator.

   -- *)
let d_dict_keyval d : (int * cff_value list * cff_key) ok =
  let rec aux stepsum vacc =
    d_dict_element d >>= fun (step, elem) ->
(*
      Format.fprintf fmtCFF "dict element: %d, %a@," step pp_element elem;  (* for debug *)
*)
      match elem with
      | Value(v) -> aux (stepsum + step) (Alist.extend vacc v)
      | Key(k)   -> return (stepsum + step, Alist.to_list vacc, k)
  in
  aux 0 Alist.empty


let d_dict len d : dict ok =
  let rec loop_keyval mapacc len d =
    if len = 0 then return mapacc else
    if len < 0 then err `Invalid_cff_inconsistent_length else
      d_dict_keyval d >>= fun (step, vlst, k) ->
      loop_keyval (mapacc |> DictMap.add k vlst) (len - step) d
  in
(*
  Format.fprintf fmtCFF "length = %d@," len;  (* for debug *)
*)
  loop_keyval DictMap.empty len d


let pp_short_key fmt key =
  let f = Format.fprintf fmt "%s" in
  match key with
  | 1  -> f "hstem"
  | 3  -> f "vstem"
  | 4  -> f "vmoveto"
  | 5  -> f "rlineto"
  | 6  -> f "hlineto"
  | 7  -> f "vlineto"
  | 8  -> f "rrcurveto"
  | 10 -> f "CALLSUBR"
  | 11 -> f "RETURN"
  | 14 -> f "endchar"
  | 18 -> f "hstemhm"
  | 19 -> f "HINTMASK"
  | 20 -> f "CNTRMASK"
  | 21 -> f "rmoveto"
  | 22 -> f "hmoveto"
  | 23 -> f "vstemhm"
  | 24 -> f "rcurveline"
  | 25 -> f "rlinecurve"
  | 26 -> f "vvcurveto"
  | 27 -> f "hhcurveto"
  | 29 -> f "CALLGSUBR"
  | 30 -> f "vhcurveto"
  | 31 -> f "hvcurveto"
  | i  -> Format.fprintf fmt "!OP(%d)" i


let pp_charstring_element fmt = function
  | ArgumentInteger(i)     -> Format.fprintf fmt "%d " i
  | ArgumentReal(r)        -> Format.fprintf fmt "%f " r
  | Operator(ShortKey(i))  -> Format.fprintf fmt "%a@," pp_short_key i
  | Operator(LongKey(i))   -> Format.fprintf fmt "OP(12 %d)@," i
  | HintMaskOperator(arg)  -> Format.fprintf fmt "HINTMASK(...)@,"
  | CntrMaskOperator(arg)  -> Format.fprintf fmt "CNTRMASK(...)@,"


let d_stem_argument (numstem : int) (cd : common_decoder) : (int * stem_argument) ok =
  let arglen =
    if numstem mod 8 = 0 then
      numstem / 8
    else
      numstem / 8 + 1
  in
  d_bytes arglen cd >>= fun arg ->
  return (arglen, arg)


type charstring_state = {
  numarg        : int;
  numstem       : int;
  used_gsubr_set : IntSet.t;
  used_lsubr_set : IntSet.t;
}


let d_charstring_element (cstate : charstring_state) (d : common_decoder) : (int * charstring_state * charstring_element) ok =
  let numarg = cstate.numarg in
  let numstem = cstate.numstem in
  let return_argument (step, cselem) =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
    Format.fprintf fmtCFF "\n  # numarg = %d ---> %d@," numarg (numarg + 1);  (* for debug *)
*)
    return (step, { cstate with numarg = numarg + 1; }, cselem)
  in
  let return_flushing_operator (step, cselem) =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
*)
    return (step, { cstate with numarg = 0; }, cselem)
  in
  let return_subroutine_operator cselem =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
    Format.fprintf fmtCFF "  # numarg = %d ---> %d@," numarg (numarg - 1);  (* for debug *)
*)
    return (1, { cstate with numarg = numarg - 1; }, cselem)

  in
  let return_stem (step, cselem) =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
    Format.fprintf fmtCFF "  # step = %d, numarg = %d@," step numarg;  (* for debug *)
    Format.fprintf fmtCFF "  # stem = %d ----> %d@," numstem (numstem + numarg / 2);  (* for debug *)
*)
    return (step, { cstate with numarg = 0; numstem = numstem + numarg / 2 }, cselem)
  in
    (* -- 'numarg' may be an odd number, but it is due to the width value -- *)
  d_uint8 d >>= function
  | ( 1 | 3 | 18 | 23) as b0 ->
    (* -- stem operators -- *)
      return_stem (1, Operator(ShortKey(b0)))

  | b0  when b0 |> is_in_range 0 9 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | (10 | 29) as b0 ->
    (* -- callsubr / callgsubr operator -- *)
      return_subroutine_operator (Operator(ShortKey(b0)))

  | 11 ->
    (* -- return operator -- *)
      let cselem = Operator(ShortKey(11)) in
(*
      Format.fprintf fmtCFF "%a" pp_charstring_element cselem;
*)
      return (1, cstate, cselem)

  | 12 ->
      d_uint8 d >>= fun b1 ->
      return_flushing_operator (2, Operator(LongKey(b1)))

  | b0  when b0 |> is_in_range 13 18 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | 19 ->
    (* -- hintmask operator -- *)
(*
      Format.fprintf fmtCFF "hintmask (numstem = %d (numarg = %d) ---> %d)@," numstem numarg (numstem + numarg / 2);  (*for debug *)
*)
        d_stem_argument (numstem + numarg / 2) d >>= fun (step, bits) ->
        return_stem (1 + step, HintMaskOperator(bits))

  | 20 ->
    (* -- cntrmask operator -- *)
(*
      Format.fprintf fmtCFF "  # cntrmask (numstem = %d (numarg = %d) ---> %d)@," numstem numarg (numstem + numarg / 2);  (*for debug *)
*)
      d_stem_argument (numstem + numarg / 2) d >>= fun (step, bits) ->
      return_stem (1 + step, CntrMaskOperator(bits))

  | b0  when b0 |> is_in_range 21 27 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | 28 ->
      d_twoscompl2 d >>= fun ret ->
      return_argument (3, ArgumentInteger(ret))

  | b0  when b0 |> is_in_range 30 31 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | b0  when b0 |> is_in_range 32 246 ->
      return_argument (1, ArgumentInteger(b0 - 139))

  | b0  when b0 |> is_in_range 247 250 ->
      d_uint8 d >>= fun b1 ->
      return_argument (2, ArgumentInteger((b0 - 247) * 256 + b1 + 108))

  | b0  when b0 |> is_in_range 251 254 ->
      d_uint8 d >>= fun b1 ->
      return_argument (2, ArgumentInteger(- (b0 - 251) * 256 - b1 - 108))

  | 255 ->
      d_twoscompl2 d >>= fun ret1 ->
      d_twoscompl2 d >>= fun ret2 ->
      let ret = float_of_int ret1 +. (float_of_int ret2) /. (float_of_int (1 lsl 16)) in
      return_argument (5, ArgumentReal(ret))

  | _ ->
      assert false
        (* -- uint8 value must be in [0 .. 255] -- *)


let err_dict_key key =
  match key with
  | ShortKey(i) -> err (`Missing_required_dict_short_key(i))
  | LongKey(i)  -> err (`Missing_required_dict_long_key(i))


let get_string stridx sid =
  let nStdString = 391 in
  if sid < nStdString then
    failwith "a standard string; remains to be supported."
  else
    try return stridx.(sid - nStdString) with
    | Invalid_argument(_) -> err (`Invalid_sid(sid))


let get_integer_opt dictmap key =
    match DictMap.find_opt key dictmap with
    | Some(Integer(i) :: []) -> return (Some(i))
    | Some(Real(fl) :: [])   -> return (Some(int_of_float fl))  (* -- rounds a float value -- *)
    | Some(_)                -> err `Invalid_cff_not_an_integer
    | None                   -> return None


let get_integer_with_default dictmap key dflt =
  get_integer_opt dictmap key >>= function
  | Some(i) -> return i
  | None    -> return dflt


let get_integer dictmap key =
  get_integer_opt dictmap key >>= function
  | Some(i) -> return i
  | None    -> err_dict_key key


let get_integer_pair_opt dictmap key =
  match DictMap.find_opt key dictmap with
  | Some(Integer(i1) :: Integer(i2) :: []) -> return (Some(i1, i2))
  | Some(Integer(i1) :: Real(fl2) :: [])   -> return (Some(i1, int_of_float fl2))
  | Some(Real(fl1) :: Integer(i2) :: [])   -> return (Some(int_of_float fl1, i2))
  | Some(Real(fl1) :: Real(fl2) :: [])     -> return (Some(int_of_float fl1, int_of_float fl2))
  | Some(_)                                -> err `Invalid_cff_not_an_integer
  | None                                   -> return None


let get_sid = get_integer


let get_real_with_default dictmap key dflt =
    match DictMap.find_opt key dictmap with
    | Some(Real(r) :: []) -> return r
    | Some(_)             -> err `Invalid_cff_not_an_integer
    | None                -> return dflt


let get_boolean_with_defautlt dictmap key dflt =
  get_integer_with_default dictmap key (if dflt then 1 else 0) >>= fun i -> return (i <> 0)


let get_iquad_opt dictmap key dflt =
  match dictmap |> DictMap.find_opt key with
  | Some(Integer(i1) :: Integer(i2) :: Integer(i3) :: Integer(i4) :: []) -> return (i1, i2, i3, i4)
  | Some(_)                                                              -> err `Invalid_cff_not_a_quad
  | None                                                                 -> return dflt


let get_ros dictmap key =
  match dictmap |> DictMap.find_opt key with
  | Some(Integer(sid1) :: Integer(sid2) :: Integer(i) :: []) -> return (sid1, sid2, i)
  | Some(_)                                                  -> err `Invalid_ros
  | None                                                     -> err `Invalid_ros


let d_single_private offset_CFF dictmap d : single_private ok =
(* -- Private DICT -- *)
  get_integer_pair_opt dictmap (ShortKey(18)) >>= fun privateopt ->
    match privateopt with
    | None ->
(*
        Format.fprintf fmtCFF "No Private DICT@,";  (* for debug *)
*)
        err `Invalid_cff_no_private_dict

    | Some(size_private, reloffset_private) ->
        let offset_private = offset_CFF + reloffset_private in
        seek_pos offset_private d >>= fun () ->
        d_dict size_private d >>= fun dictmap_private ->
        get_integer_opt          dictmap_private (ShortKey(19))   >>= fun selfoffset_lsubrs_opt ->
        get_integer_with_default dictmap_private (ShortKey(20)) 0 >>= fun default_width_x ->
        get_integer_with_default dictmap_private (ShortKey(21)) 0 >>= fun nominal_width_x ->

      (* -- Local Subr INDEX -- *)
        let lsubrsidx_res =
          match selfoffset_lsubrs_opt with
          | None ->
              return [||]

          | Some(selfoffset_lsubrs) ->
              let offset_lsubrs = offset_private + selfoffset_lsubrs in
              seek_pos offset_lsubrs d >>= fun () ->
      (*
              Format.fprintf fmtCFF "* Local Subr INDEX@,";  (* for debug *)
      *)
              d_index (CharStringData(0, 0)) d_charstring_data d
        in
        lsubrsidx_res >>= fun lsubridx ->
(*
        Format.fprintf fmtCFF "length = %d@," (Array.length lsubridx);  (* for debug *)
*)
        return { default_width_x; nominal_width_x; local_subr_index = lsubridx }


let seek_number_of_glyphs offset_CharString_INDEX (cd : common_decoder) =
  seek_pos offset_CharString_INDEX cd >>= fun () ->
  d_uint16 cd


exception Internal of error


let seek_fdarray offset_CFF offset_FDArray d : fdarray ok =
  seek_pos offset_FDArray d >>= fun () ->
  d_index DictMap.empty d_dict d >>= fun arrraw ->
  try
    let arr =
      arrraw |> Array.map (fun dictmap ->
        let res =
          d_single_private offset_CFF dictmap d
        in
        match res with
        | Error(e)       -> raise (Internal(e))
        | Ok(singlepriv) -> singlepriv
      )
    in
    return arr
  with
  | Internal(e) -> err e


let d_fdselect_format_0 nGlyphs d : fdselect ok =
  let idx = Array.make nGlyphs 0 in
  let rec aux i =
    if i >= nGlyphs then
      return (FDSelectFormat0(idx))
    else
      begin
        d_uint8 d >>= fun v ->
        idx.(i) <- v;
        aux (i + 1)
      end

  in
  aux 0


let d_fdselect_format_3 d : fdselect ok =
  let rec aux num i acc =
    if i >= num then
      d_uint16 d >>= fun gid_sentinel ->
      return (FDSelectFormat3(Alist.to_list acc, gid_sentinel))
    else
      d_uint16 d >>= fun gid ->
      d_uint8 d >>= fun v ->
      aux num (i + 1) (Alist.extend acc (gid, v))
  in
  d_uint16 d >>= fun nRanges ->
  aux nRanges 0 Alist.empty


let seek_fdselect nGlyphs offset_FDSelect d : fdselect ok =
  seek_pos offset_FDSelect d >>= fun () ->
  d_uint8 d >>= function
  | 0 -> d_fdselect_format_0 nGlyphs d
  | 3 -> d_fdselect_format_3 d
  | n -> err (`Unknown_fdselect_format(n))


let pop (stk : int Stack.t) : int ok =
  try return (Stack.pop stk) with
  | Stack.Empty -> err `Invalid_charstring


let pop_opt (stk : int Stack.t) : int option =
  try Some(Stack.pop stk) with
  | Stack.Empty -> None


let pop_all (stk : int Stack.t) : int list =
  let rec aux acc =
    if Stack.is_empty stk then acc else
      let i = Stack.pop stk in
        aux (i :: acc)
  in
    aux []


let pop_pair_opt (stk : int Stack.t) : (int * int) option =
  if Stack.length stk < 2 then None else
    let y = Stack.pop stk in
    let x = Stack.pop stk in
      Some((x, y))


let pop_4_opt (stk : int Stack.t) : (int * int * int * int) option =
  if Stack.length stk < 4 then None else
    let d4 = Stack.pop stk in
    let d3 = Stack.pop stk in
    let d2 = Stack.pop stk in
    let d1 = Stack.pop stk in
      Some((d1, d2, d3, d4))


let pop_6_opt (stk : int Stack.t) : (int * int * int * int * int * int) option =
  if Stack.length stk < 6 then None else
    let d6 = Stack.pop stk in
    let d5 = Stack.pop stk in
    let d4 = Stack.pop stk in
    let d3 = Stack.pop stk in
    let d2 = Stack.pop stk in
    let d1 = Stack.pop stk in
      Some((d1, d2, d3, d4, d5, d6))


let pop_8_opt (stk : int Stack.t) : (int * int * int * int * int * int * int * int) option =
  if Stack.length stk < 8 then None else
    let d8 = Stack.pop stk in
    let d7 = Stack.pop stk in
    let d6 = Stack.pop stk in
    let d5 = Stack.pop stk in
    let d4 = Stack.pop stk in
    let d3 = Stack.pop stk in
    let d2 = Stack.pop stk in
    let d1 = Stack.pop stk in
      Some((d1, d2, d3, d4, d5, d6, d7, d8))


let pop_iter (type a) (popf : int Stack.t -> a option) (stk : int Stack.t) : (a list) =
  let rec aux acc =
    let retopt = popf stk in
    match retopt with
    | None      -> acc  (* -- returns in the forward direction -- *)
    | Some(ret) -> aux (ret :: acc)
  in
  aux []


let is_odd_length stk =
  let len = Stack.length stk in
    len mod 2 = 1


let make_bezier (dxa, dya, dxb, dyb, dxc, dyc) = ((dxa, dya), (dxb, dyb), (dxc, dyc))


let pp = Format.fprintf

let pp_cspoint fmt (x, y) = pp fmt "(%d %d)" x y

let pp_bezier fmt (dvA, dvB, dvC) = pp fmt "%a..%a..%a" pp_cspoint dvA pp_cspoint dvB pp_cspoint dvC

let pp_int fmt x = pp fmt "%d" x

let pp_int_option fmt = function
  | None    -> pp fmt "NONE"
  | Some(i) -> pp fmt "%d" i

let pp_partial fmt (dtD, (dxE, dyE), dsF) = pp fmt "%d_(%d %d)_%d" dtD dxE dyE dsF

let pp_sep_space fmt () = pp fmt " "

let pp_cspoint_list = pp_list ~pp_sep:pp_sep_space pp_cspoint

let pp_partial_list = pp_list ~pp_sep:pp_sep_space pp_partial

let pp_parsed_charstring fmt = function
  | HStem(y, dy, csptlst)   -> pp fmt "HStem(%d %d %a)" y dy pp_cspoint_list csptlst
  | VStem(x, dx, csptlst)   -> pp fmt "VStem(%d %d %a)" x dx pp_cspoint_list csptlst
  | HStemHM(y, dy, csptlst) -> pp fmt "HStemHM(%d %d %a)" y dy pp_cspoint_list csptlst
  | VStemHM(x, dx, csptlst) -> pp fmt "VStemHM(%d %d %a)" x dx pp_cspoint_list csptlst
  | HintMask(_)             -> pp fmt "HintMask(_)"
  | CntrMask(_)             -> pp fmt "CntrMask(_)"
  | VMoveTo(dy1)            -> pp fmt "VMoveTo(%d)" dy1
  | RLineTo(csptlst)        -> pp fmt "RLineTo(%a)" pp_cspoint_list csptlst
  | HLineTo(lst)            -> pp fmt "HLineTo(%a)" (pp_list ~pp_sep:pp_sep_space pp_int) lst
  | VLineTo(lst)            -> pp fmt "VLineTo(%a)" (pp_list ~pp_sep:pp_sep_space pp_int) lst
  | RRCurveTo(bezierlst)    -> pp fmt "RRCurveTo(%a)" (pp_list ~pp_sep:pp_sep_space pp_bezier) bezierlst
  | RMoveTo((dx1, dy1))     -> pp fmt "RMoveTo(%d, %d)" dx1 dy1
  | HMoveTo(dx1)            -> pp fmt "HMoveTo(%d)" dx1
  | VVCurveTo(dtopt, lst)   -> pp fmt "VVCurveTo(%a %a)" pp_int_option dtopt pp_partial_list lst
  | HHCurveTo(dtopt, lst)   -> pp fmt "HHCurveTo(%a %a)" pp_int_option dtopt pp_partial_list lst
  | VHCurveTo(lst, dtopt)   -> pp fmt "VHCurveTo(%a %a)" pp_partial_list lst pp_int_option dtopt
  | HVCurveTo(lst, dtopt)   -> pp fmt "HVCurveTo(%a %a)" pp_partial_list lst pp_int_option dtopt
  | Flex(p1, p2, p3, p4, p5, p6, fd)   -> pp fmt "Flex(%a, %a, %a, %a, %a, %a, %d)" pp_cspoint p1 pp_cspoint p2 pp_cspoint p3 pp_cspoint p4 pp_cspoint p5 pp_cspoint p6 fd
  | HFlex(dx1, p2, dx3, dx4, dx5, dx6) -> pp fmt "HFlex(%d, %a, %d, %d, %d, %d)" dx1 pp_cspoint p2 dx3 dx4 dx5 dx6
  | HFlex1(p1, p2, dx3, dx4, p5, dx6)  -> pp fmt "HFlex1(%a, %a, %d, %d, %a, %d)" pp_cspoint p1 pp_cspoint p2 dx3 dx4 pp_cspoint p5 dx6
  | Flex1(p1, p2, p3, p4, p5, d6)      -> pp fmt "Flex1(%a, %a, %a, %a, %a, %d)" pp_cspoint p1 pp_cspoint p2 pp_cspoint p3 pp_cspoint p4 pp_cspoint p5 d6


let convert_subroutine_number (idx : subroutine_index) (i : int) =
  let arrlen = Array.length idx in
  let bias =
    if arrlen < 1240 then 107 else
      if arrlen < 33900 then 1131 else
        32768
  in
    bias + i


let access_subroutine (idx : subroutine_index) (i : int) : (int * int) ok =
  try
    let CharStringData(offset, len) = idx.(convert_subroutine_number idx i) in
    return (offset, len)
  with
  | Invalid_argument(_) -> err `Invalid_charstring


type width_state =
  | WidthLookedFor
  | WidthFound of int option


let rec parse_progress (gsubridx : subroutine_index) (lsubridx : subroutine_index) (widstate : width_state) (lenrest : int) (cstate : charstring_state) (stk : int Stack.t) (d : common_decoder) : (int * width_state * charstring_state * parsed_charstring list) ok =
  d_charstring_element cstate d >>= fun (step, cstate, cselem) ->
  let lenrest = lenrest - step in
(*
  let () =  (* for debug *)
    match widstate with
    | WidthLookedFor -> Format.fprintf fmtCFF "# X %a@," pp_charstring_element cselem;  (* for debug *)
    | WidthFound(_)  -> Format.fprintf fmtCFF "# O %a@," pp_charstring_element cselem;  (* for debug *)
  in  (* for debug *)
*)
  let return_with_width ret =
    match widstate with
    | WidthLookedFor -> let wopt = pop_opt stk in return (lenrest, WidthFound(wopt), cstate, ret)
    | WidthFound(_)  -> return (lenrest, widstate, cstate, ret)
  in

  let return_cleared ret =
    return (lenrest, widstate, cstate, ret)
  in

    match cselem with
    | ArgumentInteger(i) ->
        stk |> Stack.push i; return_cleared []

    | ArgumentReal(r) ->
        stk |> Stack.push (int_of_float r); return_cleared []

    | Operator(ShortKey(1)) ->  (* -- hstem (1) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (y, dy) :: csptlst -> return_with_width [HStem(y, dy, csptlst)]
        end

    | Operator(ShortKey(3)) ->  (* -- vstem (3) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (x, dx) :: csptlst -> return_with_width [VStem(x, dx, csptlst)]
        end

    | Operator(ShortKey(4)) ->  (* -- vmoveto (4) -- *)
        pop stk >>= fun arg ->
        return_with_width [VMoveTo(arg)]

    | Operator(ShortKey(5)) ->  (* -- rlineto (5) -- *)
        let csptlst = pop_iter pop_pair_opt stk in
        return_cleared [RLineTo(csptlst)]

    | Operator(ShortKey(6)) ->  (* -- hlineto (6) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        let firstopt = pop_opt stk in
        let flatlst = pairlst |> List.map (fun (a, b) -> [a; b]) |> List.concat in
        begin
          match firstopt with
          | None        -> return_cleared [HLineTo(flatlst)]
          | Some(first) -> return_cleared [HLineTo(first :: flatlst)]
        end

    | Operator(ShortKey(7)) ->  (* -- vlineto (7) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        let firstopt = pop_opt stk in
        let flatlst = pairlst |> List.map (fun (a, b) -> [a; b]) |> List.concat in
        begin
          match firstopt with
          | None        -> return_cleared [VLineTo(flatlst)]
          | Some(first) -> return_cleared [VLineTo(first :: flatlst)]
        end

    | Operator(ShortKey(8)) ->  (* -- rrcurveto (8) -- *)
        let tuplelst = pop_iter pop_6_opt stk in
        let bezierlst = tuplelst |> List.map make_bezier in
        return_cleared [RRCurveTo(bezierlst)]

    | Operator(ShortKey(10)) ->  (* -- callsubr (10) -- *)
        pop stk >>= fun i ->
        access_subroutine lsubridx i >>= fun (offset, len) ->
        let offset_init = cur_pos d in
        seek_pos offset d >>= fun () ->
        parse_charstring len cstate d stk gsubridx lsubridx widstate >>= fun (widstatesubr, cstate, accsubr) ->
        seek_pos offset_init d >>= fun () ->
        let cstatenew =
          { cstate with
            used_lsubr_set = cstate.used_lsubr_set |> IntSet.add (convert_subroutine_number lsubridx i);
          }
        in
        return (lenrest, widstatesubr, cstatenew, Alist.to_list accsubr)

    | Operator(ShortKey(11)) ->  (* -- return (11) -- *)
        return_cleared []

    | Operator(ShortKey(14)) ->  (* -- endchar (14) -- *)
        let lst = pop_all stk in
        begin
          match widstate with
          | WidthLookedFor ->
              begin
                match lst with
                | w :: [] -> return (lenrest, WidthFound(Some(w)), cstate, [])
                | []      -> return (lenrest, WidthFound(None), cstate, [])
                | _       ->
(*
                    Format.fprintf fmtCFF "!!endchar1@,";
                    Format.pp_print_list pp_int fmtCFF lst;
*)
                    err `Invalid_charstring
              end

          | WidthFound(_) ->
              begin
                match lst with
                | [] -> return_cleared []
                | _  ->
(*
                    Format.fprintf fmtCFF "!!endchar2@,";
                    Format.pp_print_list pp_int fmtCFF lst;
*)
                    err `Invalid_charstring
              end
        end

    | Operator(ShortKey(18)) ->  (* -- hstemhm (18) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (y, dy) :: csptlst -> return_with_width [HStemHM(y, dy, csptlst)]
        end

    | HintMaskOperator(arg) ->
        if Stack.length stk = 0 then
          return_with_width [HintMask(arg)]
        else
          let pairlst = pop_iter pop_pair_opt stk in
          begin
            match pairlst with
            | []                 -> err `Invalid_charstring
            | (x, dx) :: csptlst -> return_with_width [VStemHM(x, dx, csptlst); HintMask(arg)]
          end


    | CntrMaskOperator(arg) ->
        if Stack.length stk = 0 then
          return_with_width [CntrMask(arg)]
        else
          let pairlst = pop_iter pop_pair_opt stk in
          begin
            match pairlst with
            | []                 -> err `Invalid_charstring
            | (x, dx) :: csptlst -> return_with_width [VStemHM(x, dx, csptlst); CntrMask(arg)]
          end

    | Operator(ShortKey(21)) ->  (* -- rmoveto (21) -- *)
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_with_width [RMoveTo((dx1, dy1))]

    | Operator(ShortKey(22)) ->  (* -- hmoveto (22) -- *)
        pop stk >>= fun arg ->
        return_with_width [HMoveTo(arg)]

    | Operator(ShortKey(23)) ->  (* -- vstemhm (23) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (x, dx) :: csptlst -> return_with_width [VStemHM(x, dx, csptlst)]
        end

    | Operator(ShortKey(24)) ->  (* -- rcurveline (24) -- *)
        pop stk >>= fun dyd ->
        pop stk >>= fun dxd ->
        let tuplelst = pop_iter pop_6_opt stk in
        let bezierlst = tuplelst |> List.map make_bezier in
        return_cleared [RRCurveTo(bezierlst); RLineTo([(dxd, dyd)])]

    | Operator(ShortKey(25)) ->  (* -- rlinecurve (25) -- *)
        pop stk >>= fun dyd ->
        pop stk >>= fun dxd ->
        pop stk >>= fun dyc ->
        pop stk >>= fun dxc ->
        pop stk >>= fun dyb ->
        pop stk >>= fun dxb ->
        let pairlst = pop_iter pop_pair_opt stk in
        return_cleared [RLineTo(pairlst); RRCurveTo([((dxb, dyb), (dxc, dyc), (dxd, dyd))])]

    | Operator(ShortKey(26)) ->  (* -- vvcurveto (26) --*)
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (dya, dxb, dyb, dyc) -> (dya, (dxb, dyb), dyc)) in
        let dx1opt = pop_opt stk in
        return_cleared [VVCurveTo(dx1opt, retlst)]

    | Operator(ShortKey(27)) ->  (* -- hhcurveto (27) -- *)
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (dxa, dxb, dyb, dxc) -> (dxa, (dxb, dyb), dxc)) in
        let dy1opt = pop_opt stk in
        return_cleared [HHCurveTo(dy1opt, retlst)]

    | Operator(ShortKey(29)) ->  (* -- callgsubr (29) -- *)
        pop stk >>= fun i ->
        access_subroutine gsubridx i >>= fun (offset, len) ->
        let offset_init = cur_pos d in
        seek_pos offset d >>= fun () ->
        parse_charstring len cstate d stk gsubridx lsubridx widstate >>= fun (woptoptsubr, cstate, accsubr) ->
        seek_pos offset_init d >>= fun () ->
        let cstatenew =
          { cstate with
            used_gsubr_set = cstate.used_gsubr_set |> IntSet.add (convert_subroutine_number gsubridx i);
          }
        in
        return (lenrest, woptoptsubr, cstatenew, Alist.to_list accsubr)

    | Operator(ShortKey(30)) ->  (* -- vhcurveto (30) -- *)
        begin
          if is_odd_length stk then
            pop stk >>= fun dxf ->
            return (Some(dxf))
          else
            return None
        end >>= fun lastopt ->
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return_cleared [VHCurveTo(retlst, lastopt)]

    | Operator(ShortKey(31)) ->  (* -- hvcurveto (31) -- *)
        begin
          if is_odd_length stk then
            pop stk >>= fun dxf ->
            return (Some(dxf))
          else
            return None
        end >>= fun lastopt ->
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return_cleared [HVCurveTo(retlst, lastopt)]

    | Operator(ShortKey(i)) ->
        err `Invalid_charstring

    | Operator(LongKey(i))
        when List.mem i [9; 10; 11; 12; 14; 18; 23; 24; 26; 27; 28; 29; 30] ->
        failwith (Printf.sprintf "unsupported arithmetic operator '12 %d'" i)

    | Operator(LongKey(34)) ->  (* -- hflex (12 34) -- *)
        pop stk >>= fun dx6 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dx1 ->
        return_cleared [HFlex(dx1, (dx2, dy2), dx3, dx4, dx5, dx6)]

    | Operator(LongKey(35)) ->  (* -- flex (12 35) -- *)
        pop stk >>= fun fd ->
        pop stk >>= fun dy6 ->
        pop stk >>= fun dx6 ->
        pop stk >>= fun dy5 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dy4 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dy3 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_cleared [Flex((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4), (dx5, dy5), (dx6, dy6), fd)]

    | Operator(LongKey(36)) ->  (* -- hflex1 (12 36) -- *)
        pop stk >>= fun dx6 ->
        pop stk >>= fun dy5 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_cleared [HFlex1((dx1, dy1), (dx2, dy2), dx3, dx4, (dx5, dy5), dx6)]

    | Operator(LongKey(37)) ->  (* -- flex1 (12 37) -- *)
        pop stk >>= fun d6 ->
        pop stk >>= fun dy5 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dy4 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dy3 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_cleared [Flex1((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4), (dx5, dy5), d6)]

    | Operator(LongKey(i)) ->
        err `Invalid_charstring


and parse_charstring (len : int) (cstate : charstring_state) (d : common_decoder) (stk : int Stack.t) (gsubridx : subroutine_index) (lsubridx : subroutine_index) (widstateinit : width_state) : (width_state * charstring_state * parsed_charstring Alist.t) ok =
  let rec aux lenrest (widstateprev, cstate, acc) =
    parse_progress gsubridx lsubridx widstateprev lenrest cstate stk d >>= fun (lenrest, widstate, cstate, parsed) ->
    let accnew = Alist.append acc parsed in
    if lenrest = 0 then
      return (widstate, cstate, accnew)
    else if lenrest < 0 then
      err `Invalid_charstring
    else
      match widstate with
      | WidthLookedFor -> aux lenrest (widstateprev, cstate, accnew)
      | WidthFound(_)  -> aux lenrest (widstate, cstate, accnew)
  in
  aux len (widstateinit, cstate, Alist.empty)


let select_fd_index (fdselect : fdselect) (gid : glyph_id) : fdindex ok =
  match fdselect with
  | FDSelectFormat0(arr) ->
      begin
        try return arr.(gid) with
        | Invalid_argument(_) -> err (`Invalid_fd_select(gid))
      end

  | FDSelectFormat3(lst, gid_sentinel) ->
      if gid >= gid_sentinel then
        err (`Invalid_fd_select(gid))
      else
        let opt =
          lst |> List.fold_left (fun opt (gidc, fdi) ->
            if gidc <= gid then
              Some(fdi)
            else
              opt
          ) None
        in
        begin
          match opt with
          | None      -> err (`Invalid_fd_select(gid))
          | Some(fdi) -> return fdi
        end


let select_local_subr_index (privinfo : private_info) (gid : glyph_id) : subroutine_index ok =
  match privinfo with
  | SinglePrivate(singlepriv) ->
      return singlepriv.local_subr_index

  | FontDicts(fdarray, fdselect) ->
      select_fd_index fdselect gid >>= fun fdindex ->
      try
        let singlepriv = fdarray.(fdindex) in
        return singlepriv.local_subr_index
      with
      | Invalid_argument(_) -> err (`Invalid_fd_index(fdindex))


let charstring ((dcff, gsubridx, privinfo, offset_CharString_INDEX) : charstring_info) (gid : glyph_id) : ((int option * parsed_charstring list) option) ok =
  let d = cff_common dcff in
  let cstate = { numarg = 0; numstem = 0; used_gsubr_set = IntSet.empty; used_lsubr_set = IntSet.empty; } in
  seek_pos offset_CharString_INDEX d >>= fun () ->
  d_index_access d_charstring_data gid d >>= function
  | None ->
      return None

  | Some(CharStringData(offset, len)) ->
      let stk : int Stack.t = Stack.create () in
      seek_pos offset d >>= fun () ->
      select_local_subr_index privinfo gid >>= fun lsubridx ->
      parse_charstring len cstate d stk gsubridx lsubridx WidthLookedFor >>= function
      | (WidthLookedFor, _, acc)   -> err `Invalid_charstring
      | (WidthFound(wopt), _, acc) -> return (Some((wopt, Alist.to_list acc)))


type path_element =
  | LineTo         of cspoint
  | BezierTo       of cspoint * cspoint * cspoint


let ( +@ ) (x, y) (dx, dy) = (x + dx, y + dy)

let ( +@- ) (x, y) dx = (x + dx, y)

let ( +@| ) (x, y) dy = (x, y + dy)


let line_parity (is_horizontal_init : bool) (peacc : path_element Alist.t) lst curv =
  let rec aux is_horizontal peacc lst curv =
    match lst with
    | [] ->
        (curv, peacc)

    | dt :: tail ->
        if is_horizontal then
          let curvnew = curv +@- dt in
          aux (not is_horizontal) (Alist.extend peacc (LineTo(curvnew))) tail curvnew
        else
          let curvnew = curv +@| dt in
          aux (not is_horizontal) (Alist.extend peacc (LineTo(curvnew))) tail curvnew
  in
  aux is_horizontal_init peacc lst curv


let curve_parity (is_horizontal : bool) (peacc : path_element Alist.t) lst (dtD, dvE, dsF) dtFopt curv =
  let rec aux  is_horizontal peacc lst curv =
    match lst with
    | [] ->
        if is_horizontal then
        (* -- dtD is x-directed and dsF is y-directed -- *)
          let vD = curv +@- dtD in
          let vE = vD +@ dvE in
          let vF =
            match dtFopt with
            | None      -> vE +@| dsF
            | Some(dtF) -> vE +@ (dtF, dsF)
          in
            (vF, Alist.extend peacc (BezierTo(vD, vE, vF)))
        else
          let vD = curv +@| dtD in
          let vE = vD +@ dvE in
          let vF =
            match dtFopt with
            | None      -> vE +@- dsF
            | Some(dtF) -> vE +@ (dsF, dtF)
          in
            (vF, Alist.extend peacc (BezierTo(vD, vE, vF)))

    | (dtA, dvB, dsC) :: tail ->
        if is_horizontal then
          let vA = curv +@- dtA in
          let vB = vA +@ dvB in
          let vC = vB +@| dsC in
            aux (not is_horizontal) (Alist.extend peacc (BezierTo(vA, vB, vC))) tail vC
        else
          let vA = curv +@| dtA in
          let vB = vA +@ dvB in
          let vC = vB +@- dsC in
            aux (not is_horizontal) (Alist.extend peacc (BezierTo(vA, vB, vC))) tail vC

  in
  aux is_horizontal peacc lst curv


let flex_path curv pt1 pt2 pt3 pt4 pt5 pt6 =
  let abspt1 = curv +@ pt1 in
  let abspt2 = abspt1 +@ pt2 in
  let abspt3 = abspt2 +@ pt3 in
  let abspt4 = abspt3 +@ pt4 in
  let abspt5 = abspt4 +@ pt5 in
  let abspt6 = abspt5 +@ pt6 in
  let curvnew = abspt6 in
    (curvnew, [BezierTo(abspt1, abspt2, abspt3); BezierTo(abspt4, abspt5, abspt6)])


type path = cspoint * path_element list


let charstring_absolute (csinfo : charstring_info) (gid : glyph_id) =
  charstring csinfo gid >>= function
  | None ->
      return None

  | Some((_, pcs)) ->
      pcs |> List.fold_left (fun prevres pcselem ->
        prevres >>= fun (curv, accopt) ->
(*
        Format.fprintf fmtCFF "%a@," pp_parsed_charstring pcselem;  (* for debug *)
*)
        match pcselem with
        | HintMask(_)
        | CntrMask(_)
        | HStem(_, _, _)
        | VStem(_, _, _)
        | HStemHM(_, _, _)
        | VStemHM(_, _, _)
            -> return (curv, accopt)

        | VMoveTo(dy) ->
            let curvnew = curv +@| dy in
            begin
              match accopt with
              | None ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.empty))

              | Some(((cspt, peacc), pathacc)) ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.extend pathacc (cspt, Alist.to_list peacc)))
            end

        | HMoveTo(dx) ->
            let curvnew = curv +@- dx in
            begin
              match accopt with
              | None ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.empty))

              | Some(((cspt, peacc), pathacc)) ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.extend pathacc (cspt, Alist.to_list peacc)))
            end

        | RMoveTo(dv) ->
            let curvnew = curv +@ dv in
            begin
              match accopt with
              | None ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.empty))

              | Some(((cspt, peacc), pathacc)) ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.extend pathacc (cspt, Alist.to_list peacc)))
            end

        | RLineTo(csptlst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) =
                    csptlst |> List.fold_left (fun (curv, peacc) dv ->
                      (curv +@ dv, Alist.extend peacc (LineTo(curv +@ dv)))
                    ) (curv, peacc)
                  in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | HLineTo(lst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) = line_parity true peacc lst curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VLineTo(lst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) = line_parity false peacc lst curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | RRCurveTo(tricsptlst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) =
                    tricsptlst |> List.fold_left (fun (curv, peacc) (dvA, dvB, dvC) ->
                      let vA = curv +@ dvA in
                      let vB = vA +@ dvB in
                      let vC = vB +@ dvC in
                        (vC, Alist.extend peacc (BezierTo(vA, vB, vC)))
                    ) (curv, peacc)
                  in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VVCurveTo(dx1opt, (dy1, dv2, dy3) :: vvlst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let v1 =
                    match dx1opt with
                    | None      -> curv +@| dy1
                    | Some(dx1) -> curv +@ (dx1, dy1)
                  in
                  let v2 = v1 +@ dv2 in
                  let v3 = v2 +@| dy3 in
                  let (curvnew, peaccnew) =
                    vvlst |> List.fold_left (fun (curv, peacc) (dyA, dvB, dyC) ->
                      let vA = curv +@| dyA in
                      let vB = vA +@ dvB in
                      let vC = vB +@| dyC in
                      (vC, Alist.extend peacc (BezierTo(vA, vB, vC)))
                    ) (v3, Alist.extend peacc (BezierTo(v1, v2, v3)))
                  in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VVCurveTo(_, []) ->
            err `Invalid_charstring

        | HHCurveTo(dy1opt, (dx1, dv2, dx3) :: hhlst) ->
            let v1 =
              match dy1opt with
              | None      -> curv +@- dx1
              | Some(dy1) -> curv +@ (dx1, dy1)
            in
            let v2 = v1 +@ dv2 in
            let v3 = v2 +@- dx3 in
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                let (curvnew, peaccnew) =
                  hhlst |> List.fold_left (fun (curv, peacc) (dxA, dvB, dxC) ->
                    let vA = curv +@- dxA in
                    let vB = vA +@ dvB in
                    let vC = vB +@- dxC in
                    (vC, Alist.extend peacc (BezierTo(vA, vB, vC)))
                  ) (v3, Alist.extend peacc (BezierTo(v1, v2, v3)))
                in
                return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | HHCurveTo(_, []) ->
            err `Invalid_charstring

        | HVCurveTo(hvlst, lastopt) ->
            begin
              match (List.rev hvlst, accopt) with
              | ([], _)   -> err `Invalid_charstring
              | (_, None) -> err `Invalid_charstring
              | (last :: revmain, Some(((cspt, peacc), pathacc))) ->
                  let hvlstmain = List.rev revmain in
                  let (curvnew, peaccnew) = curve_parity true peacc hvlstmain last lastopt curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VHCurveTo(vhlst, lastopt) ->
            begin
              match (List.rev vhlst, accopt) with
              | ([], _)   -> err `Invalid_charstring
              | (_, None) -> err `Invalid_charstring
              | (last :: revmain, Some(((cspt, peacc), pathacc))) ->
                  let vhlstmain = List.rev revmain in
                  let (curvnew, peaccnew) = curve_parity false peacc vhlstmain last lastopt curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | Flex(pt1, pt2, pt3, pt4, pt5, pt6, _) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, pelstflex) = flex_path curv pt1 pt2 pt3 pt4 pt5 pt6 in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some((cspt, peaccnew), pathacc))
            end

        | HFlex(dx1, (dx2, dy2), dx3, dx4, dx5, dx6) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, pelstflex) = flex_path curv (dx1, 0) (dx2, dy2) (dx3, 0) (dx4, 0) (dx5, -dy2) (dx6, 0) in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | HFlex1((dx1, dy1), (dx2, dy2), dx3, dx4, (dx5, dy5), dx6) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let dy6 = - (dy1 + dy2 + dy5) in
                  let (curvnew, pelstflex) = flex_path curv (dx1, dy1) (dx2, dy2) (dx3, 0) (dx4, 0) (dx5, dy5) (dx6, dy6) in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | Flex1(pt1, pt2, pt3, pt4, pt5, d6) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc)), pathacc) ->
                  let (dxsum, dysum) = pt1 +@ pt2 +@ pt3 +@ pt4 +@ pt5 in
                  let (xstart, ystart) = curv in
                  let abspt1 = curv +@ pt1 in
                  let abspt2 = abspt1 +@ pt2 in
                  let abspt3 = abspt2 +@ pt3 in
                  let abspt4 = abspt3 +@ pt4 in
                  let abspt5 = abspt4 +@ pt5 in
                  let (absx5, absy5) = abspt5 in
                  let abspt6 =
                    if abs dxsum > abs dysum then
                      (absx5 + d6, ystart)
                    else
                      (xstart, absy5 + d6)
                  in
                  let curvnew = abspt6 in
                  let pelstflex = [BezierTo(abspt1, abspt2, abspt3); BezierTo(abspt4, abspt5, abspt6)] in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

      ) (return ((0, 0), None))
    >>= function
    | (_, None)                           -> return (Some([]))
    | (_, Some(((cspt, peacc), pathacc))) -> return (Some(Alist.to_list (Alist.extend pathacc (cspt, Alist.to_list peacc))))


type bbox =
  | BBoxInital
  | BBox       of csx * csx * csy * csy


let update_bbox bbox (x1, y1) (x2, y2) =
  let xminnew = min x1 x2 in
  let xmaxnew = max x1 x2 in
  let yminnew = min y1 y2 in
  let ymaxnew = max y1 y2 in
    match bbox with
    | BBox(xmin, xmax, ymin, ymax) -> BBox(min xmin xminnew, max xmax xmaxnew, min ymin yminnew, max ymax ymaxnew)
    | BBoxInital                   -> BBox(xminnew, xmaxnew, yminnew, ymaxnew)


let bezier_bbox (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  let ( ~$ ) = float_of_int in

  let bezier_point t r0 r1 r2 r3 =
    if t < 0. then r0 else
      if 1. < t then r3 else
        let c1 = ~$ (3 * (-r0 + r1)) in
        let c2 = ~$ (3 * (r0 - 2 * r1 + r2)) in
        let c3 = ~$ (-r0 + 3 * (r1 - r2) + r3) in
          int_of_float @@ (~$ r0) +. t *. (c1 +. t *. (c2 +. t *. c3))
  in

  let aux r0 r1 r2 r3 =
    let a = -r0 + 3 * (r1 - r2) + r3 in
    let b = 2 * (r0 - 2 * r1 + r2) in
    let c = -r0 + r1 in
    if a = 0 then
      [r0; r3]
    else
      let det = b * b - 4 * a * c in
      if det < 0 then
        [r0; r3]
      else
        let delta = sqrt (~$ det) in
        let t_plus  = (-. (~$ b) +. delta) /. (~$ (2 * a)) in
        let t_minus = (-. (~$ b) -. delta) /. (~$ (2 * a)) in
        [r0; r3; bezier_point t_plus r0 r1 r2 r3; bezier_point t_minus r0 r1 r2 r3]
  in

  let xoptlst = aux x0 x1 x2 x3 in
  let xmax = xoptlst |> List.fold_left max x0 in
  let xmin = xoptlst |> List.fold_left min x0 in
  let yoptlst = aux y0 y1 y2 y3 in
  let ymax = yoptlst |> List.fold_left max y0 in
  let ymin = yoptlst |> List.fold_left min y0 in
  ((xmin, ymin), (xmax, ymax))


let charstring_bbox (pathlst : path list) =
  let bbox =
    pathlst |> List.fold_left (fun bboxprev (cspt, pelst) ->
      let (_, bbox) =
        pelst |> List.fold_left (fun (v0, bboxprev) pe ->
          match pe with
          | LineTo(v1)           -> (v1, update_bbox bboxprev v0 v1)
          | BezierTo(v1, v2, v3) -> let (vb1, vb2) = bezier_bbox v0 v1 v2 v3 in (v3, update_bbox bboxprev vb1 vb2)
        ) (cspt, bboxprev)
      in
        bbox
    ) BBoxInital
  in
  match bbox with
  | BBoxInital                   -> None  (* needs reconsideration *)
  | BBox(xmin, xmax, ymin, ymax) -> (Some((xmin, xmax, ymin, ymax)))


let cff_first (dcff : cff_decoder) : cff_first ok =
  let cd = cff_common dcff in
  init_decoder cd >>= fun () ->
  seek_required_table Tag.cff cd >>= fun _ ->
  let offset_CFF = cur_pos cd in

  (* -- Header -- *)
(*
  Format.fprintf fmtCFF "* Header@,";  (* for debug *)
*)
  d_uint8 cd              >>= fun major ->
  d_uint8 cd              >>= fun minor ->
(*
  Format.fprintf fmtCFF "version = %d.%d@," major minor;  (* for debug *)
*)
  d_uint8 cd              >>= fun hdrSize ->
  d_offsize cd            >>= fun offSizeGlobal ->
  d_skip (hdrSize - 4) cd >>= fun () ->
  let header =
    {
      major   = major;
      minor   = minor;
      hdrSize = hdrSize;
      offSize = offSizeGlobal;
    }
  in

  (* -- Name INDEX (which should contain only one element) -- *)
(*
  Format.fprintf fmtCFF "* Name INDEX@,";  (* for debug *)
*)
  d_index_singleton d_bytes cd >>= fun name ->

  (* -- Top DICT INDEX (which should contain only one DICT) -- *)
(*
  Format.fprintf fmtCFF "* Top DICT INDEX@,";  (* for debug *)
*)
  d_index_singleton d_dict cd >>= fun dictmap ->

  (* -- String INDEX -- *)
(*
  Format.fprintf fmtCFF "* String INDEX@,";  (* for debug *)
*)
  d_index "(dummy)" d_bytes cd >>= fun stridx ->

  (* -- Global Subr INDEX -- *)
(*
  Format.fprintf fmtCFF "* Global Subr INDEX@,";  (* for debug *)
*)
  d_index (CharStringData(0, 0)) d_charstring_data cd >>= fun gsubridx ->
    (* FIXME; should be decoded *)

  let cff_first =
    {
      cff_header   = header;
      cff_name     = name;
      top_dict     = dictmap;
      string_index = stridx;
      gsubr_index  = gsubridx;
      offset_CFF   = offset_CFF;
    }
  in
  return cff_first


let cff (dcff : cff_decoder) : cff_info ok =
  let cd = cff_common dcff in
  cff_first dcff >>= fun cff_first ->
  let font_name = cff_first.cff_name in
  let dictmap = cff_first.top_dict in
  let stridx = cff_first.string_index in
  let gsubridx = cff_first.gsubr_index in
  let offset_CFF = cff_first.offset_CFF in
(*
  get_sid         dictmap (ShortKey(0))              >>= fun sid_version ->
  get_sid         dictmap (ShortKey(1))              >>= fun sid_notice ->
  get_sid         dictmap (LongKey(0) )              >>= fun sid_copyright ->
  get_sid         dictmap (ShortKey(2))              >>= fun sid_full_name ->
  get_sid         dictmap (ShortKey(3))              >>= fun sid_family_name ->
  get_sid         dictmap (ShortKey(4))              >>= fun sid_weight ->
*)
  get_boolean_with_defautlt dictmap (LongKey(1) ) false        >>= fun is_fixed_pitch ->
  get_integer_with_default  dictmap (LongKey(2) ) 0            >>= fun italic_angle ->
  get_integer_with_default  dictmap (LongKey(3) ) (-100)       >>= fun underline_position ->
  get_integer_with_default  dictmap (LongKey(4) ) 50           >>= fun underline_thickness ->
  get_integer_with_default  dictmap (LongKey(5) ) 0            >>= fun paint_type ->
  get_integer_with_default  dictmap (LongKey(6) ) 2            >>= fun charstring_type ->
  confirm (charstring_type = 2)
    (`Invalid_charstring_type(charstring_type))      >>= fun () ->

  (* -- have not implemented 'LongKey(7) --> font_matrix' yet; maybe it is not necessary -- *)

  get_iquad_opt        dictmap (ShortKey(5)) (0, 0, 0, 0) >>= fun font_bbox ->
  get_integer_with_default      dictmap (LongKey(8) ) 0            >>= fun stroke_width ->
  get_integer          dictmap (ShortKey(17))             >>= fun reloffset_CharString_INDEX ->
  let offset_CharString_INDEX = offset_CFF + reloffset_CharString_INDEX in
  seek_number_of_glyphs offset_CharString_INDEX cd >>= fun number_of_glyphs ->
  let pairres =
    if DictMap.mem (LongKey(30)) dictmap then
    (* -- when the font is a CIDFont -- *)
      get_ros                  dictmap (LongKey(30))      >>= fun (sid_registry, sid_ordering, supplement) ->
      get_real_with_default    dictmap (LongKey(31)) 0.   >>= fun cid_font_version ->
      get_integer_with_default dictmap (LongKey(32)) 0    >>= fun cid_font_revision ->
      get_integer_with_default dictmap (LongKey(33)) 0    >>= fun cid_font_type ->
      get_integer_with_default dictmap (LongKey(34)) 8720 >>= fun cid_count ->
      get_integer     dictmap (LongKey(36))      >>= fun reloffset_FDArray ->
      get_integer     dictmap (LongKey(37))      >>= fun reloffset_FDSelect ->
      get_string stridx sid_registry >>= fun registry ->
      get_string stridx sid_ordering >>= fun ordering ->
      let offset_FDArray = offset_CFF + reloffset_FDArray in
      let offset_FDSelect = offset_CFF + reloffset_FDSelect in
      seek_fdarray offset_CFF offset_FDArray cd >>= fun fdarray ->
      seek_fdselect number_of_glyphs offset_FDSelect cd >>= fun fdselect ->
      return (Some{
        registry; ordering; supplement;
        cid_font_version;
        cid_font_revision;
        cid_font_type;
        cid_count;
      }, FontDicts(fdarray, fdselect))
    else
    (* -- when the font is not a CIDFont -- *)
      d_single_private offset_CFF dictmap cd >>= fun singlepriv ->
      return (None, SinglePrivate(singlepriv))
  in
  pairres >>= fun (cid_info, private_info) ->
  let cff_info =
    {
      cff_first;
      font_name;
      is_fixed_pitch;
      italic_angle;
      underline_position;
      underline_thickness;
      paint_type;
      font_bbox;
      stroke_width;
      cid_info;
      number_of_glyphs;
      charstring_info = (dcff, gsubridx, private_info, offset_CharString_INDEX);
    }
  in
  return cff_info


  (* --
     `d_private_lsubr_pair_opt`: returns:

     * `Some((dictmap_private, lsubrsopt))` where:

       - `dictmap_private` is the Private DICT, and
       - `lsubrsopt : subroutine_index option` is the Local Subrs if exists

       if the Top DICT has `Private` entry (i.e. the font is a CIDFont), or

     * `None` otherwise.

     -- *)
  let d_private_lsubr_pair_opt offset_CFF dictmap_top d : ((dict * subroutine_index option) option) ok =
    get_integer_pair_opt dictmap_top (ShortKey(18)) >>= fun privateopt ->
      (* -- accesses `Private` entry in the Top DICT [CFF p.15] -- *)
    match privateopt with
    | None ->
        return None

    | Some(size_private, reloffset_private) ->
        let offset_private = offset_CFF + reloffset_private in
        seek_pos offset_private d >>= fun () ->
        d_dict size_private d >>= fun dictmap_private ->
          (* -- gets the Private DICT [CFF p.24] -- *)
        get_integer_opt dictmap_private (ShortKey(19)) >>= fun lsubroffopt ->
          (* -- accesses `Subrs` entry in the Private DICT [CFF p.24] -- *)
        match lsubroffopt with
        | None ->
            return (Some(dictmap_private, None))

        | Some(selfoffset_lsubr) ->
            let offset_lsubr = offset_private + selfoffset_lsubr in
            seek_pos offset_lsubr d >>= fun () ->
            d_index (CharStringData(0, 0)) d_charstring_data d >>= fun lsubridx ->
            return (Some(dictmap_private, Some(lsubridx)))


  (* --
     `d_fontdict_private_pair_array`: returns an array
     in which every element is a pair of Font DICT
     and the pair of its Private DICT and its Local Subrs INDEX
     --*)
  let d_fontdict_private_pair_array offset_CFF dict_Top d : ((dict * (dict * subroutine_index option) option) array) ok =
    get_integer dict_Top (LongKey(36)) >>= fun zoffset_FDArray ->
      (* -- accesses `FDArray` entry in the Top DICT [CFF p.16] -- *)
    seek_pos (offset_CFF + zoffset_FDArray) d >>= fun () ->
    d_index DictMap.empty d_dict d >>= fun arrraw ->
    try
      let arr =
        arrraw |> Array.map (fun dict_Font ->
          let res =
            d_private_lsubr_pair_opt offset_CFF dict_Font d
          in
          match res with
          | Error(e)                -> raise (Internal(e))
          | Ok(priv_lsubr_pair_opt) -> (dict_Font, priv_lsubr_pair_opt)
        )
      in
      return arr
    with
    | Internal(e) -> err e


type cff_raw_glyph = raw_glyph


let cff_raw_glyph (g : cff_raw_glyph) = g


let get_cff_raw_glyph (dcff : cff_decoder) (gid : glyph_id) : (cff_raw_glyph option) ok =
  let d = cff_common dcff in

  cff dcff >>= fun cffinfo ->
  let blank_rawg =
    {
      old_glyph_id = gid;
      glyph_aw = 0;
      glyph_lsb = 0;
      glyph_bbox = (0, 0, 0, 0);
      glyph_data = "\014"; (* 14:endchar *)
      glyph_data_length = 1;
      glyph_data_offset = None;
      glyph_composite_offsets = [];
    }
  in
  match charstring_absolute cffinfo.charstring_info gid with
  | Error(oerr) ->
     return None

  | Ok(None) ->
      return (Some(blank_rawg))
        (* needs reconsideration; maybe should emit an error *)

  | Ok(Some(pathlst)) ->
      begin
        match charstring_bbox pathlst with
        | None ->
            return (Some(blank_rawg))

        | Some(bbox_raw) ->
            let (_, _, _, offset_CharString_INDEX) = cffinfo.charstring_info in
            seek_pos offset_CharString_INDEX d >>= fun () ->
            d_index_access d_charstring_data gid d >>= function
            | None ->
                return None

            | Some(CharStringData(offset, len)) ->
                seek_pos offset d >>= fun () ->
                d_bytes len d >>= fun data ->
                let (xmin_raw, ymin_raw, xmax_raw, ymax_raw) = bbox_raw in
                hmtx_single d gid >>= fun (aw, lsb) ->
                let rawg =
                  {
                    old_glyph_id = gid;
                    glyph_aw = aw;
                    glyph_lsb = lsb;
                    glyph_bbox = (xmin_raw, ymin_raw, xmax_raw, ymax_raw);
                    glyph_data = data;
                    glyph_data_length = len;
                    glyph_data_offset = Some(offset);
                    glyph_composite_offsets = [];
                  }
                in
                return (Some(rawg))
      end
