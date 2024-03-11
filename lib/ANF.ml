(* Implementation largely stolen from
   https://github.com/stonebuddha/GuideTypes/blob/pldi21/anf.ml
*)

open Core
open AbstractSyntaxTree

type binop = AbstractSyntaxTree.binop
type 'a dist = 'a AbstractSyntaxTree.dist

type atomic_exp =
  | AE_var of string
  | AE_triv
  | AE_bool of bool
  | AE_real of float
  | AE_nat of int
  | AE_inf
  | AE_ninf
  | AE_binop of binop * atomic_exp * atomic_exp
  | AE_dist of atomic_exp dist
  | AE_pair of atomic_exp * atomic_exp
  | AE_logPr of atomic_exp * atomic_exp
  (* This is TODO *)
  | AE_field of atomic_exp * int

type complex_exp =
  | CE_cond of atomic_exp * inter_exp * inter_exp
  | CE_app of atomic_exp * atomic_exp
  | CE_logML of AbstractSyntaxTree.cmd * string
  | CE_sample of atomic_exp * string
  | CE_factor of atomic_exp * string
  (* These are all TODO *)
  | CE_case of atomic_exp * string * atomic_exp * string * atomic_exp
  | CE_inl of atomic_exp
  | CE_inr of atomic_exp

and inter_exp =
  | IE_let of (atomic_exp, complex_exp) Core.Either.t * string * inter_exp
  | IE_tail of (atomic_exp, complex_exp) Core.Either.t
  | IE_abs of string * inter_exp

type anf_top_level = ANF_pure of string * inter_exp
type anf_prog = anf_top_level list

(* ANF *)
let genvar =
  let cnt = ref 0 in
  fun () ->
    let res = "temp_" ^ Int.to_string !cnt in
    incr cnt;
    res
;;

let genmodel =
  let cnt = ref 0 in
  fun () ->
    let res = "model_" ^ Int.to_string !cnt in
    incr cnt;
    res
;;

(* AbstractSyntaxTree.exp -> ANF.inter_exp *)
let rec normalize_exp exp cont =
  match exp.exp_desc with
  | E_var var_name -> cont (Either.first (AE_var var_name.txt))
  | E_triv -> cont (Either.first AE_triv)
  | E_bool b -> cont (Either.first (AE_bool b))
  | E_real f -> cont (Either.first (AE_real f))
  | E_nat n -> cont (Either.first (AE_nat n))
  | E_inf -> cont (Either.first AE_inf)
  | E_ninf -> cont (Either.first AE_ninf)
  | E_abs (arg_name, _, body) -> IE_abs (arg_name.txt, normalize_exp body cont)
  | E_cond (cond, tbranch, fbranch) ->
    normalize_exp_name cond (fun nexp ->
      cont
        (Either.second
           (CE_cond (nexp, normalize_exp_tail tbranch, normalize_exp_tail fbranch))))
  | E_binop (bop, lhs, rhs) ->
    normalize_exp_name lhs (fun nexp1 ->
      normalize_exp_name rhs (fun nexp2 ->
        cont (Either.first (AE_binop (bop.txt, nexp1, nexp2)))))
  | E_dist d -> normalize_dist d (fun ndist -> cont (Either.first (AE_dist ndist)))
  | E_app (rator, rand) ->
    normalize_exp_name rator (fun nexp1 ->
      normalize_exp_name rand (fun nexp2 -> cont (Either.second (CE_app (nexp1, nexp2)))))
  | E_let (ev, v, e) ->
    normalize_exp ev (fun nexp -> IE_let (nexp, v.txt, normalize_exp e cont))
  | E_pair (exp1, exp2) ->
    normalize_exp_name exp1 (fun nexp1 ->
      normalize_exp_name exp2 (fun nexp2 -> cont (Either.first (AE_pair (nexp1, nexp2)))))
  | E_logPr (dist, v) ->
    normalize_exp_name dist (fun nexp1 ->
      normalize_exp_name v (fun nexp2 -> cont (Either.first (AE_logPr (nexp1, nexp2)))))
  | E_logML cmd -> cont (Either.second (CE_logML (cmd, genmodel ())))
  | E_field (_, _) -> failwith "TODO: E_field"
  | E_case (_, _, _, _, _) -> failwith "TODO: not implemented"
  | E_inl _ -> failwith "TODO: not implemented"
  | E_inr _ -> failwith "TODO: not implemented"

and normalize_dist dist cont =
  match dist with
  | D_ber exp -> normalize_exp_name exp (fun nexp -> cont (D_ber nexp))
  | D_unif -> cont D_unif
  | D_beta (exp1, exp2) ->
    normalize_exp_name exp1 (fun nexp1 ->
      normalize_exp_name exp2 (fun nexp2 -> cont (D_beta (nexp1, nexp2))))
  | D_gamma (exp1, exp2) ->
    normalize_exp_name exp1 (fun nexp1 ->
      normalize_exp_name exp2 (fun nexp2 -> cont (D_gamma (nexp1, nexp2))))
  | D_normal (exp1, exp2) ->
    normalize_exp_name exp1 (fun nexp1 ->
      normalize_exp_name exp2 (fun nexp2 -> cont (D_normal (nexp1, nexp2))))
  | D_cat exps ->
    let rec inner l c =
      match l with
      | [] -> c []
      | h :: t -> normalize_exp_name h (fun nh -> inner t (fun nt -> c (nh :: nt)))
    in
    inner exps (fun nexps -> cont (D_cat nexps))
  | D_bin (n, exp) -> normalize_exp_name exp (fun nexp -> cont (D_bin (n, nexp)))
  | D_geo exp -> normalize_exp_name exp (fun nexp -> cont (D_geo nexp))
  | D_pois exp -> normalize_exp_name exp (fun nexp -> cont (D_pois nexp))

and normalize_exp_name exp cont =
  normalize_exp exp (fun nexp ->
    Either.value_map
      nexp
      ~first:(fun aexp -> cont aexp)
      ~second:(fun _ ->
        let var_name = genvar () in
        IE_let (nexp, var_name, cont (AE_var var_name))))

and normalize_exp_tail exp = normalize_exp exp (fun nexp -> IE_tail nexp)

and normalize_cmd cmd cont =
  match cmd.cmd_desc with
  | M_bnd (var, term, comm) ->
    normalize_trm term (fun nexp1 -> IE_let (nexp1, var.txt, normalize_cmd comm cont))
  | _ -> failwith "TODO"

and normalize_trm trm _ =
  match trm.trm_desc with
  | T_ret exp -> normalize_exp_tail exp
  | T_sample exp -> normalize_exp_tail exp
  | T_factor exp -> normalize_exp_tail exp
  | _ -> failwith "TODO"

and normalize_cmd_tail cmd = normalize_cmd cmd (fun nexp -> IE_tail nexp)

(* AbstractSyntaxTree.prog -> ANF.anf_prog *)
let normalize_prog prog =
  List.filter_map prog ~f:(function
    | Top_pure (proc_name, _, body) ->
      Some (ANF_pure (proc_name.txt, normalize_exp_tail body))
    | Top_proc (_, _) -> failwith "Probabilistic functions are not allowed"
    | _ -> None)
;;
