open Core
open Level.Level
open Level.LevelExpr
open Level
open Common

let print_solution_item fmt (i, e) = Format.fprintf fmt "?%d |-> %a" i print_level_expr e
let find_uvars c = Int.Set.union_list @@ List.map c ~f:uvars_of_level_constraint

let find_uvars_in_map m =
  Int.Set.union_list @@ List.map (Map.to_alist m) ~f:(fun (_, e) -> uvars_of_level_expr e)
;;

let split_constraintes u constraintes =
  let lbs =
    List.filter constraintes ~f:(fun (Leq (lhs, rhs)) ->
      LevelExpr.equal (Leaf u) rhs && not (appears_level_expr u lhs))
  in
  let rest =
    List.filter constraintes ~f:(fun (Leq (lhs, rhs)) ->
      not (LevelExpr.equal (Leaf u) rhs && not (appears_level_expr u lhs)))
  in
  lbs, rest
;;

let gather_lb constraintes =
  let exprs = List.map constraintes ~f:(fun (Leq (lhs, _)) -> lhs) in
  let open Option.Let_syntax in
  let%bind lb = List.reduce exprs ~f:(fun lhs rhs -> Join (lhs, rhs)) in
  Some (normalize_level_expr lb)
;;

let rec solve_leq verbose todo constraintes s =
  match todo with
  | [] -> s
  | i :: todo ->
    let uvar = LV_uvar i in
    let lbs, rest = split_constraintes uvar constraintes in
    let lb = gather_lb lbs |> Option.value ~default:(Leaf (LV_const L)) in
    let lb = normalize_level_expr @@ subst_uvars_until_cannot s lb in
    if verbose
    then
      Format.printf
        "\n=====\n[split  lbs]\t %a\n[split rest]\t %a\n"
        (print_list ~f:print_level_constraint)
        lbs
        (print_list ~f:print_level_constraint)
        rest;
    let s' = Map.add_exn s ~key:i ~data:lb in
    let s'' =
      Map.map s' ~f:(fun e -> normalize_level_expr @@ subst_uvars_until_cannot s' e)
    in
    let updated = List.map rest ~f:(subst_uvars_level_constrainte s'') in
    if verbose
    then
      Format.printf
        "[subst] \t %a\n[update]\t %a -> %a\n[updated]\t %a\n"
        (print_list ~f:print_solution_item)
        (Map.to_alist s'')
        print_level
        uvar
        print_level_expr
        lb
        (print_list ~f:print_level_constraint)
        updated;
    solve_leq verbose todo rest s''
;;

let rec solve_eq solution constraintes =
  let s' =
    Map.map constraintes ~f:(fun e ->
      normalize_level_expr @@ subst_uvars_until_cannot solution e)
  in
  let solution' = Map.of_key_set (Map.key_set solution) ~f:(fun i -> Map.find_exn s' i) in
  if Set.is_empty @@ find_uvars_in_map s'
  then solution'
  else solve_eq solution' constraintes
;;

let rec sanity_check constraints solution =
  match constraints with
  | [] -> Ok ()
  | Leq (lhs, rhs) :: constraints ->
    let n_lhs = normalize_level_expr @@ subst_uvars_until_cannot solution lhs in
    let n_rhs = normalize_level_expr @@ subst_uvars_until_cannot solution rhs in
    (* Format.printf "\n%a\n%a\n" print_level_expr n_lhs print_level_expr n_rhs; *)
    (match n_lhs, n_rhs with
     | Leaf (LV_const H), Leaf (LV_const L) -> Or_error.of_exn @@ failwith "No soulution"
     | _, Leaf (LV_const H) -> sanity_check constraints solution
     | Leaf (LV_const L), _ -> sanity_check constraints solution
     | Leaf x, _ ->
       if appears_level_expr x n_rhs
       then sanity_check constraints solution
       else Or_error.of_exn @@ failwith "TODO sanity_check"
     | _, _ ->
       if LevelExpr.equal n_lhs n_rhs
       then sanity_check constraints solution
       else Or_error.of_exn @@ failwith "TODO sanity_check")
;;

let solve verbose c =
  let open Or_error.Let_syntax in
  let leq_todo = find_uvars c in
  let leq_res = solve_leq verbose (Set.to_list leq_todo) c Int.Map.empty in
  if verbose
  then
    Format.printf
      "\n=====\n[leq solution]\t %a\n"
      (print_list ~f:print_solution_item)
      (Map.to_alist leq_res);
  let eq_todo = find_uvars_in_map leq_res in
  let init = Map.of_key_set eq_todo ~f:(fun _ -> Leaf (LV_const L)) in
  let eq_res = solve_eq init leq_res in
  if verbose
  then
    Format.printf
      "\n=====\n[eq solution]\t %a\n"
      (print_list ~f:print_solution_item)
      (Map.to_alist eq_res);
  let solution = compose leq_res eq_res in
  let%bind () = sanity_check c solution in
  if verbose
  then
    Format.printf
      "\n=====\n[solution]\t %a\n"
      (print_list ~f:print_solution_item)
      (Map.to_alist solution);
  Ok solution
;;
