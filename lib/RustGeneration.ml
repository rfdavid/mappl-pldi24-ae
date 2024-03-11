open Core
open AbstractSyntaxTree
open Type
open Identifier

let indent = "    "
let header = ""
let footer = ""

(* ' cannot be used in Rust identifiers*)
let primer varname =
  let r = Str.regexp "'" in
  Str.global_replace r "_prime" varname
;;

let dump_prim_ty prim_ty =
  match prim_ty with
  | Pty_unit -> "()"
  | Pty_bool -> "bool"
  | Pty_ureal -> "f64"
  | Pty_preal -> "f64"
  | Pty_real -> "f64"
  | Pty_fnat _ -> "i32"
  | Pty_nat -> "u32"
;;

let dump_binop binop =
  match binop.txt with
  | Bop_add -> "+"
  | Bop_sub -> "-"
  | Bop_mul -> "*"
  | Bop_div -> "/"
  | Bop_eq -> "=="
  | Bop_ne -> "!="
  | Bop_lt -> "<"
  | Bop_le -> "<="
  | Bop_gt -> ">"
  | Bop_ge -> ">="
  | Bop_and -> "&&"
  | Bop_or -> "||"
;;

let rec dump_base_ty fmt bty =
  match bty.bty_desc with
  | Bty_prim pty -> Format.fprintf fmt "%s" (dump_prim_ty pty)
  | Bty_var var -> Format.fprintf fmt "%s" (primer var.txt)
  | Bty_sum (tyv1, tyv2) ->
    Format.fprintf fmt "%a + %a" dump_base_ty tyv1 dump_base_ty tyv2
  | Bty_prod (tyv1, tyv2) ->
    Format.fprintf fmt "%a * %a" dump_base_ty tyv1 dump_base_ty tyv2
  | Bty_arrow (tyv1, tyv2) ->
    Format.fprintf fmt "fn(%a) -> %a" dump_base_ty tyv1 dump_base_ty tyv2
  | Bty_dist tyv -> Format.fprintf fmt "%a" dump_base_ty tyv
;;

let rec dump_exp fmt exp =
  match exp.exp_desc with
  | E_var var_name -> Format.fprintf fmt "%s" (primer var_name.txt)
  | E_triv -> Format.fprintf fmt "()"
  | E_bool b -> Format.fprintf fmt "%B" b
  | E_real r -> Format.fprintf fmt "%f" r
  | E_nat n -> Format.fprintf fmt "%d" n
  | E_inf -> Format.fprintf fmt "f64::INFINITY"
  | E_ninf -> Format.fprintf fmt "f64::NEG_INFINITY"
  | E_cond (cond, tbranch, fbranch) ->
    Format.fprintf
      fmt
      "@[<hv>if %a {@;<1 4>@[%a@]@;} else {@;<1 4>@[%a@]@;}@]"
      dump_exp
      cond
      dump_exp
      tbranch
      dump_exp
      fbranch
  | E_binop (bop, lhs, rhs) ->
    Format.fprintf
      fmt
      "@[<hv>@[%a@] %s@;@[%a@]@]"
      dump_exp
      lhs
      (dump_binop bop)
      dump_exp
      rhs
  | E_dist d -> dump_dist fmt d
  | E_abs (arg_name, arg_type, body) ->
    Format.fprintf
      fmt
      "@[Rc::new(move |%s: %a| {@;<1 4>@[%a@]@;})@]"
      (primer arg_name.txt)
      dump_base_ty
      arg_type
      dump_exp
      body
  | E_app (rator, rand) -> Format.fprintf fmt "%a(%a)" dump_exp rator dump_exp rand
  | E_logPr (dist, v) -> Format.fprintf fmt "log_pr(%a, %a)" dump_exp dist dump_exp v
  | E_let (ev, v, e) ->
    Format.fprintf
      fmt
      "@[{@;<1 4>@[let %s = %a;@;%a@]@;}@]"
      (primer v.txt)
      dump_exp
      ev
      dump_exp
      e
  | E_pair (exp1, exp2) -> Format.fprintf fmt "%a, %a" dump_exp exp1 dump_exp exp2
  | E_field (_, _) -> failwith "TODO: E_field"
  | E_case (_, _, _, _, _) -> failwith "TODO: not implemented"
  | E_inl _ -> failwith "TODO: not implemented"
  | E_inr _ -> failwith "TODO: not implemented"
  | E_logML _ -> failwith "TODO: not implemented"

and dump_dist fmt d =
  match d with
  | D_ber e -> Format.fprintf fmt "Bernoulli::new(%a)" dump_exp e
  | D_normal (e1, e2) ->
    Format.fprintf fmt "Gaussian::new(%a, %a)" dump_exp e1 dump_exp e2
  | D_cat _ -> Format.fprintf fmt "Categorical::new(%s)" "TODO: categorical list"
  | _ -> failwith "Unsupported dist"
;;

let rec dump_bty fmt bty =
  match bty.bty_desc with
  | Bty_prim prim_ty -> Format.fprintf fmt "%s" (dump_prim_ty prim_ty)
  | Bty_var type_id -> Format.fprintf fmt "%s" (primer type_id.txt)
  | Bty_sum (left, right) | Bty_prod (left, right) ->
    Format.fprintf fmt "%a !! %a" dump_bty left dump_bty right
  | Bty_arrow (left, right) ->
    Format.fprintf fmt "@[Rc<dyn Fn(%a)@;<1 4>-> %a>@]" nest_fn left dump_bty right
  | Bty_dist base_ty -> dump_base_ty fmt base_ty

and nest_fn fmt bty =
  match bty.bty_desc with
  | Bty_arrow (left, right) ->
    Format.fprintf fmt "fn(%a) -> %a" dump_bty left dump_bty right
  | _ -> dump_bty fmt bty
;;

(* This is a wrapper for dump_exp which is the workhorse,
   this function only retrieves the argument of the first lambda *)
let fetch_first_arg_type fmt exp =
  match exp.exp_desc with
  | E_abs (_, arg_type, _) -> Format.fprintf fmt "%a" dump_base_ty arg_type
  (* If the body is an eta-reduction *)
  | E_var var -> Format.fprintf fmt "%s(_dummy)" (primer var.txt)
  | _ -> failwith "Impossible"
;;

let dump_top_level fmt = function
  | Top_pure (name, bty, body) ->
    Format.fprintf
      fmt
      "@[pub fn %s()@;<1 4>-> %a@;{@\n%s@[%a@]@\n}@]@."
      (primer name.txt)
      dump_bty
      bty
      indent
      dump_exp
      body
  | Top_proc (_, _) -> failwith "Probabilistic functions are not allowed"
  | _ -> Format.fprintf fmt ""
;;

let dump_rust fmt prog =
  Format.fprintf fmt "@[%s%a%s@]" header (Format.pp_print_list dump_top_level) prog footer
;;

(* TODO: write function that grabs first argument *)
