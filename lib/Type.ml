open Core
open Identifier

exception Type_error of string * Location.t

let () =
  Location.register_error_of_exn (function
    | Type_error (msg, loc) -> Some (Location.errorf ~loc "%s" msg)
    | _ -> None)
;;

type prim_ty =
  | Pty_unit
  | Pty_bool
  | Pty_ureal
  | Pty_preal
  | Pty_real
  | Pty_fnat of int
  | Pty_nat
[@@deriving show, equal, compare, hash]

let is_prim_subtype pty1 pty2 =
  match pty1, pty2 with
  | Pty_unit, Pty_unit -> true
  | Pty_bool, Pty_bool -> true
  | Pty_ureal, Pty_ureal | Pty_ureal, Pty_preal | Pty_ureal, Pty_real -> true
  | Pty_preal, Pty_preal | Pty_preal, Pty_real -> true
  | Pty_real, Pty_real -> true
  | Pty_fnat n, Pty_fnat m -> n <= m
  | Pty_fnat _, Pty_nat -> true
  | Pty_fnat n, Pty_ureal -> n <= 2
  | Pty_fnat _, Pty_preal | Pty_fnat _, Pty_real -> true
  | Pty_nat, Pty_nat | Pty_nat, Pty_preal | Pty_nat, Pty_real -> true
  | _ -> false
;;

let print_prim_ty fmt = function
  | Pty_unit -> Format.fprintf fmt "Unit"
  | Pty_bool -> Format.fprintf fmt "bool"
  | Pty_ureal -> Format.fprintf fmt "ureal"
  | Pty_preal -> Format.fprintf fmt "preal"
  | Pty_real -> Format.fprintf fmt "real"
  | Pty_fnat n -> Format.fprintf fmt "nat[%d]" n
  | Pty_nat -> Format.fprintf fmt "nat"
;;

type base_ty =
  { bty_desc : base_ty_desc
  ; bty_loc : Location.t
  }

and base_ty_desc =
  | Bty_prim of prim_ty
  | Bty_var of type_id
  | Bty_sum of base_ty * base_ty
  | Bty_prod of base_ty * base_ty
  | Bty_arrow of base_ty * base_ty
  | Bty_dist of base_ty
[@@deriving show, equal, compare, hash]

type base_tyv =
  | Btyv_prim of prim_ty
  | Btyv_var of string
  | Btyv_sum of base_tyv * base_tyv
  | Btyv_prod of base_tyv * base_tyv
  | Btyv_arrow of base_tyv * base_tyv
  | Btyv_dist of base_tyv
[@@deriving show, equal, compare, hash]

let mknoloc_bty bty_desc = { bty_desc; bty_loc = Location.none }

let rec to_base_ty : base_tyv -> base_ty = function
  | Btyv_prim pty -> mknoloc_bty @@ Bty_prim pty
  | Btyv_var s -> mknoloc_bty @@ Bty_var (Location.mknoloc s)
  | Btyv_sum (lhs, rhs) -> mknoloc_bty @@ Bty_sum (to_base_ty lhs, to_base_ty rhs)
  | Btyv_prod (lhs, rhs) -> mknoloc_bty @@ Bty_prod (to_base_ty lhs, to_base_ty rhs)
  | Btyv_arrow (lhs, rhs) -> mknoloc_bty @@ Bty_arrow (to_base_ty lhs, to_base_ty rhs)
  | Btyv_dist d -> mknoloc_bty @@ Bty_dist (to_base_ty d)
;;

let rec erase_loc_base_ty ty =
  match ty.bty_desc with
  | Bty_prim pty -> Btyv_prim pty
  | Bty_var var -> Btyv_var var.txt
  | Bty_sum (ty1, ty2) -> Btyv_sum (erase_loc_base_ty ty1, erase_loc_base_ty ty2)
  | Bty_prod (ty1, ty2) -> Btyv_prod (erase_loc_base_ty ty1, erase_loc_base_ty ty2)
  | Bty_arrow (ty1, ty2) -> Btyv_arrow (erase_loc_base_ty ty1, erase_loc_base_ty ty2)
  | Bty_dist dist -> Btyv_dist (erase_loc_base_ty dist)
;;

let rec print_base_tyv fmt = function
  | Btyv_prim pty -> print_prim_ty fmt pty
  | Btyv_var var -> Format.fprintf fmt "%s" var
  | Btyv_sum (tyv1, tyv2) ->
    Format.fprintf fmt "%a + %a" print_base_tyv tyv1 print_base_tyv tyv2
  | Btyv_prod (tyv1, tyv2) ->
    Format.fprintf fmt "%a * %a" print_base_tyv tyv1 print_base_tyv tyv2
  | Btyv_arrow (Btyv_arrow (a, b), tyv2) ->
    let tyv1 = Btyv_arrow (a, b) in
    Format.fprintf fmt "(%a) -> %a" print_base_tyv tyv1 print_base_tyv tyv2
  | Btyv_arrow (tyv1, tyv2) ->
    Format.fprintf fmt "%a -> %a" print_base_tyv tyv1 print_base_tyv tyv2
  | Btyv_dist tyv -> Format.fprintf fmt "%a dist" print_base_tyv tyv
;;

(* and print_base_tyv_prod fmt = function
   | Btyv_prod (tyv1, tyv2) ->
   Format.fprintf fmt "%a * %a" print_base_tyv_prim tyv1 print_base_tyv_prod tyv2
   | tyv -> print_base_tyv_prim fmt tyv

   and print_base_tyv_prim fmt = function
   | Btyv_prim pty -> print_prim_ty fmt pty
   | Btyv_dist tyv -> Format.fprintf fmt "%a dist" print_base_tyv_prim tyv
   | tyv -> Format.fprintf fmt "(%a)" print_base_tyv tyv
   ;; *)

type proc_sig =
  { psig_arg_tys : (variable_id * base_ty) list
  ; psig_ret_ty : base_ty
  }
[@@deriving show]

type proc_sigv =
  { psigv_arg_tys : (string * base_tyv) list
  ; psigv_ret_ty : base_tyv
  }
[@@deriving show]

type context = base_tyv String.Map.t

type global_context =
  { top_external_pure_sigvs : base_tyv String.Map.t
  ; top_pure_sigvs : base_tyv String.Map.t
  ; top_proc_sigvs : proc_sigv String.Map.t
  }
