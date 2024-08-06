open Core
module Build_info = Build_info.V1

let version_to_string v =
  Option.value_map ~f:Build_info.Version.to_string v ~default:"dev"
;;

let version = version_to_string (Build_info.version ())

let build_info =
  let libs =
    List.map (Build_info.Statically_linked_libraries.to_list ()) ~f:(fun lib ->
      ( Build_info.Statically_linked_library.name lib
      , version_to_string (Build_info.Statically_linked_library.version lib) ))
    |> List.sort ~compare:[%compare: string * string]
  in
  let max_length =
    List.fold_left libs ~init:0 ~f:(fun n (name, _) -> max n (String.length name))
  in
  String.concat
    ~sep:"\n"
    ((Printf.sprintf "%-*s %s" (max_length + 2) "ocaml:" Sys.ocaml_version
      :: "statically linked libraries:"
      :: List.map libs ~f:(fun (name, v) -> Printf.sprintf "- %-*s %s" max_length name v)
     )
     @ [ "version:" ])
;;

let report_result result =
  Or_error.iter_error result ~f:(fun err ->
    let exn = Error.to_exn err in
    Format.eprintf "@.";
    try
      Format.eprintf "%a" Location.report_exception exn;
      exit 1
    with
    | _ ->
      Format.eprintf "%a@." Error.pp err;
      exit 1)
;;

let parse_file ?(log_time = false) filename =
  MAPPL.Timer.wrap_duration ~log_time "parsing" (fun () ->
    match Sys_unix.file_exists filename with
    | `No | `Unknown -> Error (Error.of_string "file not found")
    | `Yes ->
      let parse_channel ch =
        let lexbuf = Lexing.from_channel ch in
        Location.init lexbuf filename;
        Location.input_name := filename;
        Location.input_lexbuf := Some lexbuf;
        MAPPL.Parse.implementation lexbuf
      in
      In_channel.with_file filename ~f:parse_channel)
;;

let info_type_check ?(log_time = false) verbose prog =
  MAPPL.Timer.wrap_duration ~log_time "typechecking" (fun () ->
    MAPPL.InformationTyping.tycheck_prog verbose prog)
;;

let plain_type_check ?(log_time = false) verbose prog =
  MAPPL.Timer.wrap_duration ~log_time "typechecking" (fun () ->
    MAPPL.Typing.tycheck_prog verbose prog)
;;

let var_elim ?(log_time = false) verbose prog =
  MAPPL.Timer.wrap_duration ~log_time "variable elimination" (fun () ->
    MAPPL.VariableElimination.elim_prog verbose prog)
;;

let cmd_only_parse =
  Command.basic
    ~summary:"only parse"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and ast = flag "-ast" no_arg ~doc:"dump ast"
     and print = flag "-print" no_arg ~doc:"pretty print" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind prog = parse_file filename in
         if ast then Format.printf "%s\n" (MAPPL.AbstractSyntaxTree.show_prog prog);
         if print then Format.printf "%a@\n" MAPPL.AbstractSyntaxTree.print_prog prog;
         Ok prog
       in
       report_result result)
;;

let cmd_info_check =
  Command.basic
    ~summary:"info check"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and verbose = flag "-verbose" no_arg ~doc:"verbose logging for debug"
     and level_count = flag "-u" no_arg ~doc:"print number of unification variables" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind prog = parse_file filename in
         let%bind result = info_type_check verbose prog in
         if level_count
         then Format.printf "Unification variables@.%d@." !MAPPL.Level.level_variable;
         Ok result
       in
       report_result result)
;;

let cmd_type_check =
  Command.basic
    ~summary:"type check"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and verbose = flag "-verbose" no_arg ~doc:"verbose logging for debug" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind prog = parse_file filename in
         let%bind result = plain_type_check verbose prog in
         Ok result
       in
       report_result result)
;;

let cmd_var_elim =
  Command.basic
    ~summary:"variable elimination"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and verbose = flag "-verbose" no_arg ~doc:"verbose logging for debug"
     and hoist = flag "-hoist" no_arg ~doc:"apply lambda hoisting when it is on"
     and log_time = flag "-time" no_arg ~doc:"count running time when it is on"
     and level_count = flag "-u" no_arg ~doc:"print number of unification variables"
     and output = flag "-output" (optional Filename_unix.arg_type) ~doc:" output file" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind parsed = parse_file ~log_time filename in
         let%bind elimed = var_elim ~log_time verbose parsed in
         if verbose
         then
           Format.printf "[cmd_var_elim]@.%a@." MAPPL.AbstractSyntaxTree.print_prog elimed;
         let%bind optimized =
           elimed
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.EtaConversion.eta_conversion_prog
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.PuretoFunc.pure_to_func
         in
         if verbose
         then
           Format.printf "[optimized]@.%a@." MAPPL.AbstractSyntaxTree.print_prog optimized;
         let%bind prog =
           if hoist
           then (
             (* let%bind prehoisted = MAPPL.PuretoFunc.pure_to_func optimized in *)
             (* Format.printf "[prehoisted]@\n%a@\n" MAPPL.AbstractSyntaxTree.print_prog prehoisted; *)
             (* Format.print_flush (); *)
             let%bind hoisted = MAPPL.Hoisting.hoist optimized in
             Ok hoisted)
           else Ok optimized
         in
         if level_count
         then Format.printf "Unification variables@.%d@." !MAPPL.Level.level_variable
         else (
           match output with
           | Some output ->
             Out_channel.with_file output ~f:(fun ch ->
               let fmt = Format.formatter_of_out_channel ch in
               Format.fprintf fmt "%a@\n" MAPPL.AbstractSyntaxTree.print_prog prog;
               Format.pp_print_flush fmt ())
           | None -> Format.printf "%a@\n" MAPPL.AbstractSyntaxTree.print_prog prog);
         Ok prog
       in
       report_result result)
;;

let cmd_rust =
  Command.basic
    ~summary:"rust generation from mappl source"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and verbose = flag "-verbose" no_arg ~doc:"verbose logging for debug"
     and log_time = flag "-time" no_arg ~doc:"count running time when it is on"
     and output = flag "-output" (optional Filename_unix.arg_type) ~doc:" output file" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind parsed = parse_file ~log_time filename in
         let%bind elimed = var_elim ~log_time verbose parsed in
         if verbose
         then
           Format.printf "[cmd_var_elim]@.%a@." MAPPL.AbstractSyntaxTree.print_prog elimed;
         let%bind optimized =
           elimed
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.EtaConversion.eta_conversion_prog
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.PuretoFunc.pure_to_func
         in
         if verbose
         then
           Format.printf "[optimized]@.%a@." MAPPL.AbstractSyntaxTree.print_prog optimized;
         let%bind prog = MAPPL.Hoisting.hoist optimized in
         (match output with
          | Some output ->
            Out_channel.with_file output ~f:(fun ch ->
              let fmt = Format.formatter_of_out_channel ch in
              Format.fprintf fmt "%a@." MAPPL.RustGeneration.dump_rust prog;
              Format.pp_print_flush fmt ())
          | None -> Format.printf "%a@." MAPPL.RustGeneration.dump_rust prog);
         Ok prog
       in
       report_result result)
;;

let cmd_dump_rust =
  Command.basic
    ~summary:"rust code generation from hoisted input"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type) in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind prog = parse_file filename in
         Format.printf "[cmd_dump_rust]@.%a" MAPPL.RustGeneration.dump_rust prog;
         Ok prog
       in
       report_result result)
;;

let cmd_pyro =
  Command.basic
    ~summary:"show generating function from mappl source"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and verbose = flag "-verbose" no_arg ~doc:"verbose logging for debug"
     and log_time = flag "-time" no_arg ~doc:"count running time when it is on"
     and output = flag "-output" (optional Filename_unix.arg_type) ~doc:" output file" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind parsed = parse_file ~log_time filename in
         let%bind elimed = var_elim ~log_time verbose parsed in
         if verbose
         then
           Format.printf "[cmd_var_elim]@.%a@." MAPPL.AbstractSyntaxTree.print_prog elimed;
         let%bind optimized =
           elimed
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.EtaConversion.eta_conversion_prog
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.PuretoFunc.pure_to_func
         in
         if verbose
         then
           Format.printf "[optimized]@.%a@." MAPPL.AbstractSyntaxTree.print_prog optimized;
         let%bind prog = MAPPL.Hoisting.hoist optimized in
         let anf_prog = MAPPL.ANF.normalize_prog prog in
         (match output with
          | Some output ->
            Out_channel.with_file output ~f:(fun ch ->
              let fmt = Format.formatter_of_out_channel ch in
              Format.fprintf fmt "%a@." MAPPL.PyroGeneration.dump_pyro anf_prog;
              Format.pp_print_flush fmt ())
          | None -> Format.printf "%a@." MAPPL.PyroGeneration.dump_pyro anf_prog);
         Ok prog
       in
       report_result result)
;;

let cmd_gf =
  Command.basic
    ~summary:"pyro generation from mappl source"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and verbose = flag "-verbose" no_arg ~doc:"verbose logging for debug"
     and log_time = flag "-time" no_arg ~doc:"count running time when it is on"
     and output = flag "-output" (optional Filename_unix.arg_type) ~doc:" output file" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind parsed = parse_file ~log_time filename in
         let%bind elimed = var_elim ~log_time verbose parsed in
         if verbose
         then
           Format.printf "[cmd_var_elim]@.%a@." MAPPL.AbstractSyntaxTree.print_prog elimed;
         let%bind optimized =
           elimed
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.EtaConversion.eta_conversion_prog
           |> MAPPL.ConstantPropagation.const_propagation_prog
           |> MAPPL.PuretoFunc.pure_to_func
         in
         if verbose
         then
           Format.printf "[optimized]@.%a@." MAPPL.AbstractSyntaxTree.print_prog optimized;
         let%bind prog = MAPPL.Hoisting.hoist optimized in
         let anf_prog = MAPPL.ANF.normalize_prog prog in
         (match output with
          | Some output ->
            Out_channel.with_file output ~f:(fun ch ->
              let fmt = Format.formatter_of_out_channel ch in
              Format.fprintf fmt "%a@." MAPPL.GeneratingFunction.dump_pyro anf_prog;
              Format.pp_print_flush fmt ())
          | None -> Format.printf "%a@." MAPPL.GeneratingFunction.dump_pyro anf_prog);
         Ok prog
       in
       report_result result)
;;

let cmd_dump_pyro =
  Command.basic
    ~summary:"perform ANF conversion into pyro code generation"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and output = flag "-output" (optional Filename_unix.arg_type) ~doc:" output file" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind prog = parse_file filename in
         let anf_prog = MAPPL.ANF.normalize_prog prog in
         (match output with
          | Some output ->
            Out_channel.with_file output ~f:(fun ch ->
              let fmt = Format.formatter_of_out_channel ch in
              Format.fprintf fmt "%a@\n" MAPPL.PyroGeneration.dump_pyro anf_prog;
              Format.pp_print_flush fmt ())
          | None -> Format.printf "%a@\n" MAPPL.PyroGeneration.dump_pyro anf_prog);
         Ok anf_prog
       in
       report_result result)
;;

let cmd_hoist =
  Command.basic
    ~summary:"lambda hoisting"
    (let open Command.Let_syntax in
     let%map_open filename = anon ("filename" %: Filename_unix.arg_type)
     and output = flag "-output" (optional Filename_unix.arg_type) ~doc:" output file" in
     fun () ->
       let result =
         let open Or_error.Let_syntax in
         let%bind prog = parse_file filename in
         let%bind prehoisted = MAPPL.PuretoFunc.pure_to_func prog in
         let%bind hoisted = MAPPL.Hoisting.hoist prehoisted in
         (match output with
          | Some output ->
            Out_channel.with_file output ~f:(fun ch ->
              let fmt = Format.formatter_of_out_channel ch in
              Format.fprintf fmt "%a@\n" MAPPL.AbstractSyntaxTree.print_prog hoisted;
              Format.pp_print_flush fmt ())
          | None -> Format.printf "%a@\n" MAPPL.AbstractSyntaxTree.print_prog hoisted);
         Ok hoisted
       in
       report_result result)
;;

let cmd_route =
  Command.group
    ~summary:"mappl compiler"
    [ "only-parse", cmd_only_parse
    ; "type-check", cmd_type_check
    ; "info-check", cmd_info_check
    ; "dump-rust", cmd_dump_rust
    ; "rust", cmd_rust
    ; "dump-pyro", cmd_dump_pyro
    ; "pyro", cmd_pyro
    ; "gf", cmd_gf
    ; "var-elim", cmd_var_elim
    ; "hoist", cmd_hoist
    ]
;;

let () =
  (*
     let t1 = Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch in
     at_exit (fun () ->
     let t2 =
     Time_now.nanoseconds_since_unix_epoch () |> Time_ns.of_int63_ns_since_epoch
     in
     Format.printf "@.total time: %a@." Time_ns.Span.pp Time_ns.(diff t2 t1));
  *)
  Command_unix.run ~version ~build_info cmd_route
;;
