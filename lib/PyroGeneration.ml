open Core
open ANF
open AbstractSyntaxTree
open Type
open Identifier
open Common

let indent = "    "
let dummy_param = "_x"

let header =
  "import torch\n\
   import pyro\n\
   import pyro.distributions as dist\n\
   from functools import cache\n\
   import argparse\n"
;;

let footer =
  "\n\
   def main():\n\
  \    parser = argparse.ArgumentParser()\n\
  \    args = parser.parse_args()\n\n\
   if __name__ == '__main__':\n\
  \    main()\n"
;;

(* Decide if a top-level should be memoized or not, inner lambdas
   always are so they don't need to invoke this function *)
let memoize_decider func_name =
  if String.is_prefix ~prefix:"lambda" func_name then "@cache\n" else ""
;;

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

let rec dump_atomic fmt = function
  | AE_var var_name -> Format.fprintf fmt "%s" (primer var_name)
  | AE_triv -> Format.fprintf fmt "0.0"
  | AE_bool b -> Format.fprintf fmt "%s" (if b then "True" else "False")
  | AE_real r -> Format.fprintf fmt "%f" r
  | AE_nat n -> Format.fprintf fmt "%d" n
  | AE_inf -> Format.fprintf fmt "float('inf')"
  | AE_ninf -> Format.fprintf fmt "float('-inf')"
  | AE_binop (bop, lhs, rhs) ->
    Format.fprintf
      fmt
      "@[<hv>@[%a@]%t@[%s %a@]@]"
      dump_atomic
      lhs
      (* Python is a fake language and needs backslashes for line breaks *)
      (Format.pp_print_custom_break ~fits:("", 0, " ") ~breaks:(" \\", 0, ""))
      (dump_binop bop)
      dump_atomic
      rhs
  | AE_dist d -> dump_dist fmt d
  | AE_pair (exp1, exp2) -> Format.fprintf fmt "%a, %a" dump_atomic exp1 dump_atomic exp2
  | AE_logPr (dist, v) ->
    Format.fprintf fmt
      "%a.log_prob(torch.tensor(%a, dtype=torch.float))"
      dump_atomic dist
      dump_atomic v
  | AE_array lst -> Format.fprintf fmt "[%a]" (print_list ~f:dump_atomic) lst
  | AE_field (_, _) -> failwith "TODO: AE_field"

and dump_dist fmt = function
  | D_ber e -> Format.fprintf fmt "dist.Bernoulli(%a)" dump_atomic e
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
  | Some None -> Format.fprintf fmt "return %a"
;;

let rec dump_complex ?bind fmt = function
  | CE_cond (cond, tbranch, fbranch) ->
    Format.fprintf
      fmt
      "@[<hv>if %a:@\n%s@[%a@]@\nelse:@\n%s@[%a@]@\n@]"
      dump_atomic
      cond
      indent
      (dump_inter ?bind)
      tbranch
      indent
      (dump_inter ?bind)
      fbranch
  | CE_app (rator, rand) ->
    return_or_bind
      ?bind
      fmt
      (fun fmt () -> Format.fprintf fmt "%a(%a)" dump_atomic rator dump_atomic rand)
      ()
  | CE_logML (iexp, _) ->
    return_or_bind
      ?bind
      fmt
      (fun fmt () ->
        Format.fprintf
          fmt
          "logML(@\n%s@[def %s():@\n%s@[%a@]@]@;)@;"
          indent
          (genmodel ())
          indent
          (dump_inter ?bind)
          iexp)
      ()
  | CE_call (name, args) ->
    return_or_bind ?bind fmt (fun fmt () -> emit_call fmt name args) ()
  | CE_sample exp ->
    return_or_bind
      ?bind
      fmt
      (fun fmt () ->
        Format.fprintf fmt "pyro.sample(%s, %a)" (gensample ()) dump_atomic exp)
      ()
  | CE_factor exp ->
    return_or_bind
      ?bind
      fmt
      (fun fmt () ->
        Format.fprintf fmt "pyro.factor(%s, %a)" (genfactor ()) dump_atomic exp)
      ()
  | CE_observe (obs, dist) ->
    return_or_bind
      ?bind
      fmt
      (fun fmt () ->
        Format.fprintf fmt "pyro.sample(%s, %a, obs=%a)" 
        (gensample ()) 
        dump_atomic dist
        dump_atomic obs)
      ()
  | _ -> failwith "TODO: sample, observe, etc"

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
  | IE_let (Second (CE_logML (iexp, var)), v, e) ->
    Format.fprintf
      fmt
      "@[def %s():@\n%s@[%a@]@]@\n%s = logML(%s)@\n%a"
      var
      indent
      (dump_inter ?bind)
      iexp
      v
      var
      (dump_inter ?bind)
      e
  | IE_let (ev, v, e) ->
    dump_either ~bind:(Some v) fmt ev;
    dump_inter ?bind fmt e
  | IE_abs (arg_name, body) ->
    let arg_name = primer arg_name in
    let func_name = func_prefix arg_name in
    Format.fprintf
      fmt
      (* Define a unary lambda and return it, unapplied *)
      "@[@@cache@\ndef %s(@[%s@]):@\n%s@[%a@]@\nreturn %s@]"
      func_name
      arg_name
      indent
      (dump_inter ?bind)
      body
      func_name
  | IE_tail (Second (CE_logML (iexp, var))) ->
    Format.fprintf
      fmt
      "@[def %s():@\n%s@[%a@]@]@\nreturn logML(%s)"
      var
      indent
      (dump_inter ?bind)
      iexp
      var
  | IE_tail atoplex -> dump_either ?bind fmt atoplex

and dump_either ?bind fmt = function
  | First atom ->
    return_or_bind ?bind fmt (fun fmt () -> Format.fprintf fmt "%a" dump_atomic atom) ()
  | Second comp -> dump_complex ?bind fmt comp
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
  | ANF_pure (id, body) ->
    Format.fprintf
      fmt
      "@[%sdef %s(%s):@\n%s@[%a@]@]@."
      (memoize_decider id)
      id
      dummy_param
      indent
      dump_tree
      body
  | ANF_func (id, sign, body) ->
    Format.fprintf
      fmt
      "@[%sdef %s(%a):@\n%s@[%a@]@]@."
      (memoize_decider id)
      id
      emit_sig
      sign
      indent
      (dump_inter ~bind:None)
      body
;;

let dump_pyro fmt anf_prog =
  Format.fprintf fmt "@[%s@.%a@]" header (Format.pp_print_list dump_top_level) anf_prog
;;

(* TODO: only cache functions used in LogSumExp *)
