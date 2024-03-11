open Core
open Option.Let_syntax
open Common

type security =
  | L
  | H
[@@deriving show, equal, compare, hash, sexp]

let print_security fmt = function
  | L -> Format.fprintf fmt "L"
  | H -> Format.fprintf fmt "H"
;;

module Level = struct
  type t =
    | LV_const of security
    | LV_var of int
    | LV_uvar of int
  [@@deriving show, equal, compare, hash, sexp]
end

open Level

let print_level fmt = function
  | LV_const s -> Format.fprintf fmt "%a" print_security s
  | LV_var var -> Format.fprintf fmt "%d" var
  | LV_uvar var -> Format.fprintf fmt "?%d" var
;;

module LevelExpr = struct
  type t =
    | Leaf of Level.t
    | Join of t * t
    | Subst of t * (int * t) list (* level_expr [ level_expr / vars] *)
  [@@deriving show, equal, compare, hash, sexp]
end

open LevelExpr
module LevelExprSet = Core.Set.Make (LevelExpr)

let rec print_level_expr fmt = function
  | Leaf l -> Format.fprintf fmt "%a" print_level l
  | Join (lhs, rhs) ->
    Format.fprintf fmt "%a ⊔ %a" print_level_expr lhs print_level_expr rhs
  | Subst (l, s) ->
    Format.fprintf fmt "%a[%a]" print_level_expr l (print_list ~f:print_subst_item) s

and print_subst_item fmt (i, e) = Format.fprintf fmt "%a / %d" print_level_expr e i

let is_L = function
  | Leaf (LV_const L) -> true
  | _ -> false
;;

let is_H = function
  | Leaf (LV_const H) -> true
  | _ -> false
;;

let is_const = function
  | Leaf (LV_const _) -> true
  | _ -> false
;;

let set_to_level_expr s = List.reduce (Set.to_list s) ~f:(fun acc e -> Join (e, acc))

let rec uvars_of_level_expr = function
  | Leaf (LV_const _) -> Int.Set.empty
  | Leaf (LV_var _) -> Int.Set.empty
  | Leaf (LV_uvar u) -> Int.Set.singleton u
  | Join (lhs, rhs) -> Set.union (uvars_of_level_expr lhs) (uvars_of_level_expr rhs)
  | Subst (e, s) ->
    let uvars_in_s =
      Int.Set.union_list @@ List.map s ~f:(fun (_, e') -> uvars_of_level_expr e')
    in
    Set.union uvars_in_s (uvars_of_level_expr e)
;;

let rec appears_level_expr x = function
  | Leaf l -> Level.equal l x
  | Join (lhs, rhs) -> appears_level_expr x lhs || appears_level_expr x rhs
  | Subst (e, s) ->
    appears_level_expr x e || List.exists s ~f:(fun (_, e') -> appears_level_expr x e')
;;

let rec subst_leve_expr s = function
  | Leaf (LV_const l) -> Leaf (LV_const l)
  | Leaf (LV_var i) -> Leaf (LV_uvar (Map.find_exn s i))
  | Leaf (LV_uvar _) -> failwith "TODO"
  | Join (lhs, rhs) -> Join (subst_leve_expr s lhs, subst_leve_expr s rhs)
  | Subst _ -> failwith "TODO"
;;

let rec subst_vars_only s = function
  | Leaf (LV_var i) -> Map.find_exn s i
  | Leaf l -> Leaf l
  | Join (lhs, rhs) -> Join (subst_vars_only s lhs, subst_vars_only s rhs)
  | Subst _ -> failwith "subst_vars_only"
;;

let rec subst_uvars_only s = function
  | Leaf (LV_const l) -> Leaf (LV_const l)
  | Leaf (LV_var i) -> Leaf (LV_var i)
  | Leaf (LV_uvar i) ->
    (match Map.find s i with
     | None -> Leaf (LV_uvar i)
     | Some e -> if appears_level_expr (LV_uvar i) e then Leaf (LV_uvar i) else e)
  | Join (lhs, rhs) -> Join (subst_uvars_only s lhs, subst_uvars_only s rhs)
  | Subst (e, sgima) ->
    let e' = subst_uvars_only s e in
    let sigma' = List.map sgima ~f:(fun (u, exp) -> u, subst_uvars_only s exp) in
    Subst (e', sigma')
;;

let rec normalize_level_expr_imp = function
  | Leaf l -> LevelExprSet.singleton (Leaf l)
  | Join (lhs, rhs) ->
    let lhs = normalize_level_expr_imp lhs in
    let rhs = normalize_level_expr_imp rhs in
    let merged = Set.union lhs rhs in
    if Set.exists merged ~f:is_H
    then LevelExprSet.singleton @@ Leaf (LV_const H)
    else Set.filter merged ~f:(Fn.compose not is_L)
    (* Might returns empty set *)
  | Subst (e, s) ->
    let e = Option.value_exn (set_to_level_expr @@ normalize_level_expr_imp e) in
    if is_const e
    then LevelExprSet.singleton e
    else (
      let s =
        List.filter_map s ~f:(fun (i, exp) ->
          let%bind e = set_to_level_expr @@ normalize_level_expr_imp exp in
          Some (i, e))
      in
      if Set.is_empty (uvars_of_level_expr e)
      then (
        let s = Int.Map.of_alist_exn s in
        normalize_level_expr_imp @@ subst_uvars_only s e)
      else LevelExprSet.singleton (Subst (e, s)))
;;

let normalize_level_expr e =
  set_to_level_expr @@ normalize_level_expr_imp e
  |> Option.value ~default:(Leaf (LV_const L))
;;

let rec subst_uvars_until_cannot s e =
  let e' = normalize_level_expr @@ subst_uvars_only s e in
  if LevelExpr.equal e e' then e else subst_uvars_until_cannot s e'
;;

let rec update_uvars_only s = function
  | Leaf (LV_const l) -> Leaf (LV_const l)
  | Leaf (LV_var i) -> Leaf (LV_var i)
  | Leaf (LV_uvar i) -> Subst (Leaf (LV_uvar i), s)
  | Join (lhs, rhs) -> Join (update_uvars_only s lhs, update_uvars_only s rhs)
  | Subst _ -> failwith "TODO"
;;

let rec list_to_level_expr l =
  match l with
  | [] -> failwith "impossible"
  | x :: [] -> x
  | x :: xs -> Join (x, list_to_level_expr xs)
;;

type level_constraint = Leq of LevelExpr.t * LevelExpr.t
[@@deriving show, equal, compare, hash]

let subst_uvars_level_constrainte s (Leq (lhs, rhs)) =
  Leq (subst_uvars_only s lhs, subst_uvars_only s rhs)
;;

let uvars_of_level_constraint = function
  | Leq (lhs, rhs) -> Set.union (uvars_of_level_expr lhs) (uvars_of_level_expr @@ rhs)
;;

let print_join_sep fmt () = Format.fprintf fmt "⊔"
let print_join_list ~f = Format.pp_print_list ~pp_sep:print_join_sep f

let compose x y =
  Map.map x ~f:(fun e -> normalize_level_expr @@ subst_uvars_until_cannot y e)
;;

let print_level_constraint fmt = function
  | Leq (lhs, rhs) ->
    Format.fprintf fmt "%a <= %a" print_level_expr lhs print_level_expr rhs
;;

let level_variable = ref 0
let reset_level_variable () = level_variable := 0

let gen_new_level_var_index () =
  let c = !level_variable in
  if c > 100 then failwith "[gen_new_level_var] too many vars";
  incr level_variable;
  c
;;

let gen_new_level_var () = LV_var (gen_new_level_var_index ())
let gen_new_unification_var () = LV_uvar (gen_new_level_var_index ())
