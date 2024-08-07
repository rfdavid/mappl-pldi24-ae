open Core
open ANF
open AbstractSyntaxTree
open Type
open Identifier
open Common

let indent = "    "
let dummy_param = "_x"

let header =
  "Generating Function\n"
;;

let gf_expression : string ref = ref ""

let mult_gf value =
  gf_expression := !gf_expression ^ " * " ^ value

let print_gf () = print_endline !gf_expression

(* Hacky string replacement for legal identifiers *)
let primer varname =
  if String.equal varname "lambda"
  then "lambda_"
  else (
    let r = Str.regexp "'" in
    Str.global_replace r "_prime" varname)
;;

let func_prefix varname = "f_of_" ^ varname

let gensample =
  let cnt = ref 0 in
  fun () ->
    let res = "\"sample_" ^ Int.to_string !cnt ^ "\"" in
    incr cnt;
    res
;;

let genfactor =
  let cnt = ref 0 in
  fun () ->
    let res = "\"factor_" ^ Int.to_string !cnt ^ "\"" in
    incr cnt;
    res
;;

let dump_binop binop =
  match binop with
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
  | Bop_and -> "and"
  | Bop_or -> "or"
;;


let extract_real_value = function
  | AE_real r -> r
  | _ -> 0.0

(* --- 

   DUMP ATOMIC 

   --- *)
let rec dump_atomic fmt = function
  | AE_var var_name -> Format.fprintf fmt "%s" (primer var_name)
  | AE_triv -> Format.fprintf fmt "0.0"
  | AE_bool b -> Format.fprintf fmt "%s" (if b then "True" else "False")
  | AE_real r -> Format.fprintf fmt "%f" r
  | AE_nat n -> Format.fprintf fmt "%d" n
  | AE_inf -> Format.fprintf fmt "float('inf')"
  | AE_ninf -> Format.fprintf fmt "float('-inf')"
  | AE_binop (bop, lhs, rhs) ->
    print_endline ("[*] AE_binop: ");

    (* Debugging purposes *)
    let lhs_desc = match rhs with
        | AE_var v -> "Variable: " ^ v
        | AE_real r -> "Real number: " ^ string_of_float r
        | AE_bool b -> "Boolean: " ^ string_of_bool b
        | _ -> "Other type"
    in
    print_endline ("[*] AE_binop, lhs is " ^ lhs_desc);
    print_endline ("[*] AE_binop" ^ dump_binop bop);

    Format.fprintf
      fmt
      "@[<hv>@[%a@]%t@[%s %a@]@]"
      dump_atomic
      lhs
      (Format.pp_print_custom_break ~fits:("", 0, " ") ~breaks:(" \\", 0, ""))
      (dump_binop bop)
      dump_atomic
      rhs
  | AE_dist d -> 
      print_endline ("[*] AE_dist");
      dump_dist fmt d
  | AE_pair (exp1, exp2) -> Format.fprintf fmt "%a, %a" dump_atomic exp1 dump_atomic exp2
  | AE_logPr (dist, v) ->
    print_endline ("[*] AE_logPr");
    Format.fprintf fmt
      " --- %a => [%a] --- "
      dump_atomic dist
      dump_atomic v
  | AE_array lst -> Format.fprintf fmt "[%a]" (print_list ~f:dump_atomic) lst
  | AE_field (_, _) -> failwith "TODO: AE_field"

and dump_dist fmt = function
  | D_ber e -> 
    let r = 1.0 -. extract_real_value e in
    let m = "(" ^ string_of_float(extract_real_value e) ^ "x) + " ^ string_of_float(r) in
    mult_gf(m);
    Format.fprintf fmt " *** %s *** " m
  | D_unif -> Format.fprintf fmt "dist.Uniform???"
  | D_beta (e1, e2) ->
    Format.fprintf fmt "dist.Beta(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_gamma (e1, e2) ->
    Format.fprintf fmt "dist.Gamma(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_normal (e1, e2) ->
    Format.fprintf fmt "dist.Normal(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_cat lst ->
    Format.fprintf fmt "dist.Categorical(@;<0 4>torch.tensor(%a)@;<0>)" dump_atomic lst
  | D_bin (n, exp) -> Format.fprintf fmt "dist.Binomial(%d, %a)" n dump_atomic exp
  | D_geo exp -> Format.fprintf fmt "dist.Geometric(%a)" dump_atomic exp
  | D_pois exp -> Format.fprintf fmt "dist.Poisson(%a)" dump_atomic exp
  | D_exp exp -> Format.fprintf fmt "dist.Exponential(%a)" dump_atomic exp
;;

let return_or_bind ?bind fmt =
  match bind with
  | None -> Format.fprintf fmt "%a"
  | Some (Some var) -> Format.fprintf fmt "@[%s = %a@]@\n" (primer var)
  | Some None -> Format.fprintf fmt "%a"
;;


(* --- 

   DUMP COMPLEX 

   --- *)
let rec dump_complex ?bind fmt = function
  | CE_call (name, args) ->
    print_endline ("[*] CE_call");
    return_or_bind ?bind fmt (fun fmt () -> emit_call fmt name args) ()
  | _ -> failwith "TODO: branching, sample, observe, etc"


(* Is the call a partial application of a hoisted lambda? *)
and emit_call fmt func_id args =
  let func_name = primer func_id in
  if String.is_prefix ~prefix:"lambda" func_name
  then
    (* TODO: partial application through a global context telling how many params a func takes,
       currently we assume all partial applications return unary functions *)
    Format.fprintf
      fmt
      "cached_partial(%s%s%a)"
      func_name
      (if List.length args = 0 then "" else ", ")
      (print_list ~f:dump_atomic)
      args
  else Format.fprintf fmt "%s(%a)" func_name (print_list ~f:dump_atomic) args

and dump_inter ?bind fmt = function
  | IE_let (ev, v, e) ->
    print_endline ("[*] func_name: " ^ v);
    dump_either ~bind:(Some v) fmt ev;
    dump_inter ?bind fmt e
  | IE_abs (_, _) -> ()
  | IE_tail atoplex -> 
    print_endline ("[*] IE_tail");
    dump_either ?bind fmt atoplex

and dump_either ?bind fmt = function
  | First atom ->
    print_endline ("[*] first");
    return_or_bind ?bind fmt (fun fmt () -> Format.fprintf fmt "%a" dump_atomic atom) ()
  | Second comp -> 
    print_endline ("[*] second");
    dump_complex ?bind fmt comp
;;

(* This is a wrapper for dump_inter/complex/atomic which are the workhorses,
   this function only special cases the first lambda encountered *)
let dump_tree fmt = function
  | IE_abs (arg_name, body) ->
    let arg_name = primer arg_name in
    let func_name = func_prefix arg_name in
    Format.fprintf
      fmt
      "@[@@cache@\ndef %s(@[%s@]):@\n%s@[%a@]@\nreturn %s(%s)@]"
      func_name
      arg_name
      indent
      (dump_inter ~bind:None)
      body
      func_name
      dummy_param
  (* If the body is an eta-reduction *)
  | IE_tail eta -> Format.fprintf fmt "@[%a@]" (dump_either ~bind:None) eta
  | other -> dump_inter fmt other
;;

let emit_param fmt = function
  | id, _ -> Format.fprintf fmt "%s" (primer id.txt)
;;

let emit_sig fmt sign = print_list ~f:emit_param fmt sign.psig_arg_tys

let dump_top_level fmt = function
  | ANF_pure (_,_) -> ()
  | ANF_func (id, sign, body) ->
    (* call 3 times here *)
    Format.fprintf
      fmt
      "@[%sdef %s(%a):@\n%s@[%a@]@]@."
      id
      id
      emit_sig
      sign
      indent
      (dump_inter ~bind:None)
      body
;;

let calc_gf fmt anf_prog =
  Format.fprintf fmt "@[%s@.%a@]" header (Format.pp_print_list dump_top_level) anf_prog
;;
