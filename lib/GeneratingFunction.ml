open Core
open ANF
open AbstractSyntaxTree
open Type
open Identifier
open Common

(* FADBADml *)
module Op = Fadbad.OpFloat (* elementary floating point arithmetic *)
module F = Fadbad.F(Op) (* equivalent of F<float> class *)

let random_vars : (string, string) Hashtbl.t = Hashtbl.create (module String) ~size:10

let update_rnd_var key value =
  Hashtbl.set random_vars ~key ~data:value;
;;

let print_all_rnd_vars () =
  Hashtbl.iteri random_vars ~f:(fun ~key ~data -> 
    Printf.printf "Var: %s, Value: %s\n" key data
)

let func x y =
  let open F in
  (y * x)

let dfdx v exp =
    "dgf/d" ^ v ^ "" ^ exp

let print_dfdx v exp =
    Printf.printf "%s\n" (dfdx v exp)

let header =
  "Generating Function\n"
;;

let gf_expression : string ref = ref ""

let mult_gf (value : string) : unit =
  match !gf_expression with
  | "" -> gf_expression := value
  | _ -> gf_expression := !gf_expression ^ " * " ^ value

let print_gf () = print_endline !gf_expression

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

let extract_var_name_from_ae_var = function
  | AE_var var_name -> var_name
  | _ -> ""

(* --- 

   DUMP ATOMIC 

   --- *)
let rec dump_atomic fmt = function
  | AE_var var_name -> Format.fprintf fmt "%s" (var_name)
  | AE_triv -> Format.fprintf fmt "0.0"
  | AE_bool b -> Format.fprintf fmt "%s" (if b then "True" else "False")
  | AE_real r -> Format.fprintf fmt "%f" r
  | AE_nat n -> Format.fprintf fmt "%d" n
  | AE_inf -> Format.fprintf fmt "float('inf')"
  | AE_ninf -> Format.fprintf fmt "float('-inf')"
  | AE_binop (Bop_eq, _, _) -> ()
  | AE_binop (bop, lhs, rhs) ->
    print_endline ("[*] AE_binop: ");
    (* Debugging purposes *)
    let lhs_desc = match lhs with
        | AE_var v -> "Variable: " ^ v
        | AE_real r -> "Real number: " ^ string_of_float r
        | AE_bool b -> "Boolean: " ^ string_of_bool b
        | _ -> "Other type"
    in
    let rhs_desc = match rhs with
        | AE_var v -> "Variable: " ^ v
        | AE_real r -> "Real number: " ^ string_of_float r
        | AE_bool b -> "Boolean: " ^ string_of_bool b
        | _ -> "Other type"
    in
    print_endline ("[*] AE_binop, lhs is " ^ lhs_desc);
    print_endline ("[*] AE_binop, rhs is " ^ rhs_desc);
    print_endline ("[*] AE_binop bop: " ^ dump_binop bop);
    Format.fprintf
        fmt
        "@[<hv>@[%a@]"
        dump_atomic lhs;
  | AE_dist d -> 
      print_endline ("[*] AE_dist");
      dump_dist fmt d
  | AE_pair (exp1, exp2) -> Format.fprintf fmt "%a, %a" dump_atomic exp1 dump_atomic exp2
  (*| AE_logPr (AE_dist D_pois e, v) -> () *)
  | AE_logPr (AE_dist D_ber e, v) ->
      let r = 1.0 -. extract_real_value e in
      let m = "(" ^ string_of_float(extract_real_value e) ^ extract_var_name_from_ae_var v ^ " + " ^ string_of_float(r) ^ ")" in
      update_rnd_var (extract_var_name_from_ae_var v) m;
      print_endline ("gf = " ^ m);
      print_dfdx (extract_var_name_from_ae_var v) m;
      (* print_all_rnd_vars (); *)
      mult_gf(m);
  | AE_logPr (AE_dist D_pois AE_real exp, v) ->
      (* e^(lambda(x-1)) *)
      let var = (extract_var_name_from_ae_var v) in
      let m = "(" ^ "e^(" ^ string_of_float(exp) ^ "(" ^ var ^ " - 1))" in
      mult_gf(m);
      print_endline ("gf = " ^ m);
      print_dfdx (extract_var_name_from_ae_var v) m;
  | AE_logPr (dist, v) ->
          print_endline ("[*] AE_logPr");
    Format.fprintf fmt
      "%a => [%a]"
      dump_atomic dist
      dump_atomic v
  | AE_array lst -> Format.fprintf fmt "[%a]" (print_list ~f:dump_atomic) lst
  | AE_field (_, _) -> failwith "TODO: AE_field"

and dump_dist fmt = function
  | D_ber _ -> ()
    (* let r = 1.0 -. extract_real_value e in
    let m = "(" ^ string_of_float(extract_real_value e) ^ "x) + " ^ string_of_float(r) in
    mult_gf(m); *)
  | D_unif -> Format.fprintf fmt "dist.Uniform???"
  | D_beta (e1, e2) ->
    Format.fprintf fmt "dist.Beta(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_gamma (e1, e2) ->
    Format.fprintf fmt "dist.Gamma(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_normal (e1, e2) ->
    Format.fprintf fmt "dist.Normal(%a, %a)" dump_atomic e1 dump_atomic e2
  | D_cat lst ->
    Format.fprintf fmt "dist.Categorical(@;<0 4>(%a)@;<0>)" dump_atomic lst
  | D_bin (n, exp) -> Format.fprintf fmt "dist.Binomial(%d, %a)" n dump_atomic exp
  | D_geo exp -> Format.fprintf fmt "dist.Geometric(%a)" dump_atomic exp
  | D_pois exp -> Format.fprintf fmt "%a" dump_atomic exp
  | D_exp exp -> Format.fprintf fmt "dist.Exponential(%a)" dump_atomic exp
;;

let return_or_bind ?bind fmt =
  match bind with
  | None -> Format.fprintf fmt "%a"
  | Some (Some var) -> Format.fprintf fmt "@[%s = %a@]" (var)
  | Some None -> Format.fprintf fmt "%a"
;;

let rec dump_inter ?bind fmt = function
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
  | Second _-> ()
;;

let dump_tree fmt = function
  | IE_abs (_, _) -> ()
  | IE_tail eta -> Format.fprintf fmt "@[%a@]" (dump_either ~bind:None) eta
  | other -> dump_inter fmt other
;;

let emit_param fmt = function
  | id, _ -> Format.fprintf fmt "%s" (id.txt)
;;

let emit_sig fmt sign = print_list ~f:emit_param fmt sign.psig_arg_tys

let dump_top_level fmt = function
  | ANF_pure (_,_) -> ()
  | ANF_func (_, _, body) ->
    Format.fprintf
      fmt
      "%a"
      (dump_inter ~bind:None)
      body
;;

let calc_gf fmt anf_prog =
  Format.fprintf fmt "@[%s@.%a@]" header (Format.pp_print_list dump_top_level) anf_prog;
  (* print_endline("observe"); *)
;;
