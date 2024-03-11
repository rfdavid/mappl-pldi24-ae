open Core
open ANF
open AbstractSyntaxTree

let indent = "    "
let dummy_param = "_"

let header =
  "import torch\n\
   import pyro.distributions as dist\n\
   from functools import cache\n\
   import argparse\n\n"
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

(* Replace apostrophes with _prime for legal identifiers *)
let primer varname =
  let logsumexp = Str.regexp "LogSumExp" in
  let r = Str.regexp "'" in
  let torch_lse = Str.global_replace logsumexp "LogSumExp" varname in
  Str.global_replace r "_prime" torch_lse
;;

let func_prefix varname = "f_of_" ^ varname

let gensample =
  let cnt = ref 0 in
  fun () ->
    let res = "sample_" ^ Int.to_string !cnt in
    incr cnt;
    res
;;

let genfactor =
  let cnt = ref 0 in
  fun () ->
    let res = "factor_" ^ Int.to_string !cnt in
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
    (* Custom break, Python is a fake language and needs backslashes for continuations*)
    Format.fprintf fmt
      "@[<hv>@[%a@]%t@[%s %a@]@]"
      dump_atomic lhs
      (Format.pp_print_custom_break ~fits:("", 0, " ") ~breaks:(" \\", 0, ""))
      (dump_binop bop)
      dump_atomic rhs
  | AE_dist d -> dump_dist fmt d
  | AE_pair (exp1, exp2) -> Format.fprintf fmt "%a, %a" dump_atomic exp1 dump_atomic exp2
  | AE_logPr (dist, v) ->
    Format.fprintf fmt
      "%a.log_prob(torch.tensor(%a, dtype=torch.float))"
      dump_atomic dist
      dump_atomic v
  | AE_field (_, _) -> failwith "TODO: AE_field"
  
and dump_dist fmt = function
  | D_ber e -> Format.fprintf fmt "dist.Bernoulli(%a)" dump_atomic e
  | D_normal (e1, e2) ->
    Format.fprintf fmt "dist.Normal(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_cat _ -> Format.fprintf fmt "dist.Categorical(%s)" "TODO: list for categorical"
  | _ -> failwith "Unsupported dist"
;;

let return_or_bind ?bind fmt =
  match bind with
  | None -> Format.fprintf fmt "%a"
  | Some (Some var) -> Format.fprintf fmt "@[%s = %a@]@\n" (primer var)
  | Some None -> Format.fprintf fmt "return %a"

let rec dump_complex ?bind fmt = function
  | CE_cond (cond, tbranch, fbranch) ->
    Format.fprintf fmt
      "@[<hv>if %a:@\n%s@[%a@]@;else:@\n%s@[%a@]@;@]"
      dump_atomic cond
      indent
      (dump_inter ?bind) tbranch
      indent
      (dump_inter ?bind) fbranch
  | CE_app (rator, rand) -> 
    return_or_bind ?bind fmt (fun fmt () -> 
      Format.fprintf fmt "%a(%a)" 
      dump_atomic rator 
      dump_atomic rand
    ) ()
  | CE_logML (cmd, _) -> 
    return_or_bind ?bind fmt (fun fmt () -> 
      Format.fprintf fmt "logML(@\n%s@[def %s():@\n%s@[%a@]@]@;)@;"
      indent 
      (genmodel ())
      indent
      dump_cmd cmd 
    ) () 
  |  _ -> failwith "TODO: not implemented"

and dump_inter ?bind fmt = function
  | IE_let(Second (CE_logML(cmd, var)), v, e) ->
    Format.fprintf fmt "@[def %s():@\n%s@[%a@]@]@\n%s = logML(%s)@\n%a"
    var
    indent
    dump_cmd cmd
    v var
    (dump_inter ?bind) e
  | IE_let (ev, v, e) ->
    dump_either ~bind:(Some v) fmt ev;
    dump_inter ?bind fmt e
  | IE_abs (arg_name, body) ->
    let arg_name = primer arg_name in
    let func_name = func_prefix arg_name in
    Format.fprintf fmt
      (* Define a unary lambda and return it, unapplied *)
      "@[@@cache@\ndef %s(@[%s@]):@\n%s@[%a@]@\nreturn %s@]"
      func_name arg_name
      indent
      (dump_inter ?bind) body
      func_name
  | IE_tail (Second (CE_logML(cmd, var))) ->
    Format.fprintf fmt "@[def %s():@\n%s@[%a@]@]@\nreturn logML(%s)"
    var
    indent
    dump_cmd cmd
    var
  | IE_tail atoplex -> dump_either ?bind fmt atoplex

and dump_either ?bind fmt = function
  | First atom -> return_or_bind ?bind fmt (fun fmt () -> 
      Format.fprintf fmt "%a" dump_atomic atom
    ) ()
  | Second comp -> dump_complex ?bind fmt comp

and dump_exp fmt exp =
  match exp.exp_desc with
  | E_var var_name -> Format.fprintf fmt "%s" var_name.txt
  | E_triv -> Format.fprintf fmt "()"
  | E_real r -> Format.fprintf fmt "%f" r
  | E_nat n -> Format.fprintf fmt "%d" n
  | E_dist d -> dump_dist fmt d
  | E_app (rator, rand) -> Format.fprintf fmt "%a(%a)" dump_exp rator dump_exp rand
  | E_binop (op, lhs, rhs) -> Format.fprintf fmt "%a %s %a" 
    dump_exp lhs
    (dump_binop op.txt)
    dump_exp rhs
  | E_field (exp0, field) -> Format.fprintf fmt "%a[%d]" dump_exp exp0 field
  | E_logPr (dist, v) ->
    Format.fprintf fmt "%a.log_prob(%a)" dump_exp dist dump_exp v
  | _ -> failwith "TODO"

and dump_dist fmt = function 
  | D_normal (e1, e2) -> Format.fprintf fmt "dist.Normal(%a, %a)" dump_exp e1 dump_exp e2
  | D_beta (e1, e2) -> Format.fprintf fmt "dist.Beta(%a, %a)" dump_exp e1 dump_exp e2
  | D_ber e -> Format.fprintf fmt "dist.Bernoulli(%a)" dump_exp e
  | _ -> failwith "TODO"
and dump_cmd fmt cmd =
  match cmd.cmd_desc with
  | M_trm trm -> Format.fprintf fmt "%a" dump_trm trm
  | M_seq (trm, cmd) -> Format.fprintf fmt "%a@\n%a" dump_trm trm dump_cmd cmd
  | M_bnd (var, trm, cmd) -> Format.fprintf fmt "%s = %a@\n%a" (primer var.txt) dump_trm trm dump_cmd cmd

and dump_trm fmt trm =
  match trm.trm_desc with 
  | T_ret exp -> dump_exp fmt exp
  | T_sample exp -> Format.fprintf fmt "pyro.sample(f\"%s\", %a)" (gensample ()) dump_exp exp
  | T_factor exp -> Format.fprintf fmt "pyro.factor(f\"%s\", %a)" (genfactor ()) dump_exp exp
  | _ -> failwith "TODO"
;;


(* This is a wrapper for dump_inter/complex/atomic which are the workhorses,
   this function only special cases the first lambda encountered *)
let dump_tree fmt = function
  | IE_abs (arg_name, body) ->
    let arg_name = primer arg_name in
    let func_name = func_prefix arg_name in
    Format.fprintf fmt
      "@[@@cache@\ndef %s(@[%s@]):@\n%s@[%a@]@\nreturn %s(%s)@]"
      func_name arg_name
      indent
      (dump_inter ~bind:None) body
      func_name dummy_param
  (* If the body is an eta-reduction *)
  | IE_tail eta -> Format.fprintf fmt "@[%a(%s)@]" 
    (dump_either ~bind:None) eta dummy_param
  | other -> dump_inter fmt other
;;

let dump_top_level fmt = function
  | ANF_pure (id, body) ->
    Format.fprintf fmt
      "@[%sdef %s(%s):@\n%s@[%a@]@]@."
      (memoize_decider id)
      id dummy_param
      indent
      dump_tree body
;;

let dump_pyro fmt anf_prog =
  Format.fprintf fmt "@[%s%a@]"
    header
    (Format.pp_print_list dump_top_level)
    anf_prog
;;

(* TODO: only cache functions used in LogSumExp *)
