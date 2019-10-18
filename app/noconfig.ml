(* PT is out target parsetree.
   Because all versions of OCaml vary ever so slightly, it is not
   easy to write portable code that works on the internal AST representation
   and have it compiling across many OCaml versions.
   Migrate_parsetree aims to solve this problem for us! *)
module PT = Migrate_parsetree.OCaml_409
module Parsetree = PT.Ast.Parsetree

let rec long_unwind acc =
  let open PT.Ast.Longident in
  function
  | Lident plain -> String.concat "." @@ plain :: acc
  | Ldot (x,y) -> long_unwind (y :: acc) x
  | Lapply (x, _y) -> long_unwind ("complex" :: acc) x

let modtyp =
  let open PT.Ast.Parsetree in
  function
  | None -> "NONETODO"
  | Some x ->
    match x.pmty_desc with
    | Pmty_ident {txt ; _ } -> (* long_unwind [] txt *)
      begin match txt with
        | Lident plain -> plain
        | Ldot (a, b) ->
          begin match a with
            | Lident "Mirage_types" | Lident "Mirage_types_lwt" -> ""
            | Lident sth -> sth ^ "."
            | _ -> assert false
          end ^ b
        | Lapply _ -> assert false
      end
    | _ -> "TODO too complicated"

let parse_job =
  let open Parsetree in
  let rec fold_function acc pvb_expr =
    match pvb_expr.pexp_desc with
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_any; _}, exp) ->
      fold_function ("_" :: acc) exp
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var name; _}, exp) ->
      fold_function (name.Asttypes.txt :: acc) exp
    | _ -> acc
  in
  let rec fold_functor functors = function
    | Pmod_functor (name, typ , next) ->
      (*fold_functor ((name, typ) :: acc) next.pmod_desc*)
      fold_functor ((name.txt, modtyp typ) :: functors) next.pmod_desc
    | Pmod_structure lst ->
      let start_args =
        List.fold_left (fun acc si ->
            match si.pstr_desc with
            | Pstr_value (_rec, vbs) ->
              List.fold_left (fun acc vb ->
                  (* a value binding list *)
                  begin match vb.pvb_pat.ppat_desc with
                    | Ppat_var {txt; _} ->
                      if txt <> "start" then acc else fold_function [] vb.pvb_expr
                    | _ -> acc
                  end) acc vbs
            | _ -> acc) [] lst
        |>  List.rev
      in
      begin match start_args with
        | [] -> None
        | args -> Some (args, List.rev functors)
      end
    | _ -> None
  in
  function
  | { Parsetree.pstr_desc = Pstr_module { pmb_name = job_name ;
                                          pmb_expr ; _} ; _ } ->
    begin match pmb_expr.pmod_desc with
      | Pmod_functor _ as first ->
        begin match fold_functor [] first with
          | None -> None
          | Some (args, functors) -> Some (job_name.txt, args, functors)
        end
      | _ -> None
    end
  | _ -> None

let collect_attributes things =
  let open Parsetree in
  List.fold_left (fun (keys, packages) si ->
      match si.pstr_desc with
      | Pstr_attribute { attr_name ; attr_payload ; _ } ->
        begin match attr_name.Asttypes.txt with
          | "package" -> (keys, attr_payload :: packages)
          | "key" -> (attr_payload :: keys, packages)
          | x ->
            Printf.printf "skipping unknown attribute %s\n" x;
            (keys, packages)
        end
      | _ -> (keys, packages))
    ([], []) things

let extract_package =
  let open Parsetree in
  let const_string = function
    | Pexp_constant (Pconst_string (s, _)) -> s
    | _ -> assert false
  in
  let struct_string = function
    | Pstr_eval (e, _) -> const_string e.pexp_desc
    | _ -> assert false
  in
  function
  | PStr str ->
    List.fold_left (fun acc si ->
        match si.pstr_desc with
        | Pstr_eval (e, _) ->
          let name = const_string e.pexp_desc in
          let a = e.pexp_attributes in
          let min =
            match List.find_opt (fun x -> x.attr_name.txt = "min") a with
            | None -> None
            | Some attr -> match attr.attr_payload with
              | PStr str -> Some (struct_string ((List.hd str).pstr_desc))
              | _ -> assert false
          in
          let max =
            match List.find_opt (fun x -> x.attr_name.txt = "max") a with
            | None -> None
            | Some attr -> match attr.attr_payload with
              | PStr str -> Some (struct_string ((List.hd str).pstr_desc))
              | _ -> assert false
          in
          (name, min, max) :: acc
        | _ -> acc)
      [] str
  | _ -> assert false

let topl_functors lst =
  List.iter (
    fun x -> x.Parsetree.pstr_desc |> function
    | Parsetree.Pstr_module {pmb_name ; _} ->
      Fmt.pr "module: %S\n" pmb_name.txt
    | _ -> ()
  )lst

let ext_mirage_depend parsed =
  let module D = Ocaml_common.Depend in
  D.add_implementation (D.make_leaf "TODO" |>
                        function Node (_, bound_map) -> bound_map
                       ) parsed ;
  let their_stringset = !D.free_structure_names in
  let module SS = Set.Make(struct
      type t = string let compare = compare
    end) in
  let elems = SS.elements (Obj.magic their_stringset) in
  List.filter (fun x -> String.sub x 0
                  (min 7 @@ String.length x) = "Mirage_")
    elems

let map_foreign = function
  | "PCLOCK" -> "pclock"
  | "MCLOCK" -> "mclock"
  | "KV_RO" | "Mirage_kv_lwt.RO" -> "kv_ro"
  | "HTTP" -> "http"
  | "NETWORK" -> "network"
  | "ETHERNET" -> "ethernet"
  | "ARP" -> "arpv4"
  | "IPV4" -> "ipv4"
  | "TIME" | "Mirage_time_lwt.S" -> "time"
  | "STACKV4" | "Mirage_stack_lwt.V4" -> "stackv4"
  | "CONSOLE" -> "console"
  | "BLOCK" -> "block"
  | "Resolver_lwt.S" -> "resolver"
  | "Conduit_mirage.S" -> "conduit"
  | "IPV6" -> "ipv6"
  | "RANDOM" -> "random"
  | _ -> assert false

(* and this is the actual hard part, need to keep some environment to construct proper terms *)
let map_register = function
  | _, "PCLOCK" -> "default_posix_clock"
  | _, "MCLOCK" -> "default_monotonic_clock"
  | _, "HTTP" -> "cohttp_server (conduit_direct ~tls:true (generic_stackv4 default_network))" (* TODO *)
  | name, ("KV_RO" | "Mirage_kv_lwt.RO") -> "generic_kv_ro " ^ String.lowercase_ascii name (* TODO *)
  | _, ("TIME" | "Mirage_time_lwt.S") -> "default_time"
  | _, ("STACKV4" | "Mirage_stack_lwt.V4") -> "generic_stackv4 default_network" (* TODO *)
  | _, "CONSOLE" -> "default_console"
  | _, "BLOCK" -> "block_of_file \"disk.img\"" (* TODO *)
  | _, "NETWORK" -> "default_network"
  | _, "ETHERNET" -> "etif default_network" (* TODO depending on (a) number of ethernet (b) whether network was used as well, reuse network binding *)
  | _, "ARP" -> "arp (etif default_network)" (* TODO see above *)
  | _, "IPV4" -> "create_ipv4 (etif default_network) (arp (etif default_network))" (* TODO same as above *)
  | _, "Resolver_lwt.S" -> "resolver_dns (generic_stack default_network)" (* TODO same as above*)
  | _, "Conduit_mirage.S" -> "conduit_direct (generic_stack default_network)" (* TODO same as above *)
  | _, "IPV6" -> "create_ipv6 (etif default_network) { addresses = [] ; netmasks = [] ; gateways = [] }" (* TODO ??? *)
  | _, "RANDOM" -> "default_random"
  | _ -> assert false

let pp_package ppf (name, min, max) =
  Fmt.pf ppf "package %a %a %S"
    Fmt.(option ~none:(unit "") (prefix (unit "~min:") (quote string))) min
    Fmt.(option ~none:(unit "") (prefix (unit "~max:") (quote string))) max
    name

let output_config filename name _args functors packages =
  (* assert (List.length functors = List.length args); *)
  Fmt.pr "open Mirage\n";
  Fmt.pr "let packages = [ %a ]\n\n"
    Fmt.(list ~sep:(unit "; ") pp_package) packages;
  Fmt.pr "let main = foreign ~packages %S (%a @-> job)\n\n"
    (String.capitalize_ascii (filename ^ "." ^ name))
    Fmt.(list ~sep:(unit " @-> ") string)
    (List.map map_foreign (List.map snd functors));
  Fmt.pr "let () = register %S [ main $ %a ]\n"
    (String.lowercase_ascii name)
    Fmt.(list ~sep:(unit " $ ") string)
    (List.map map_register functors)

let cmd_everything () unikernel_file =
  let original, parsed =
    (* parse using the current compiler tooling,
       then convert to the {!PT} representation that this application
       knows about: *)
    let impl =
      let lexbuf = Lexing.from_channel (open_in_bin unikernel_file) in
      Ocaml_common.Parse.implementation lexbuf in
    let module Cool = Migrate_parsetree.Convert
        (Migrate_parsetree.OCaml_current)
        (PT) in
    impl, Cool.copy_structure impl
  in
  topl_functors parsed ;
(*  Fmt.pr "%a"
      Ocaml_common.Printast.implementation _original ;*)


  let jobs = List.fold_left (fun acc expr ->
      match parse_job expr with
      | None -> acc
      | Some job -> job::acc
    ) [] parsed in

  Fmt.pr "\nExternal modules:\n%a\n----\n"
    Fmt.(list ~sep:(unit"\n")string) @@ ext_mirage_depend original ;

  let _keys, packages = collect_attributes parsed in
  let packages = List.flatten @@ List.map extract_package packages in

  List.iter (fun (job_name, args, functors) ->
      Fmt.pr "\n-- Job %S in %S:\n" job_name Sys.argv.(1);
      Fmt.pr "args: %a\nfunctors %a\n"
        Fmt.(list ~sep:(unit " ") string) args
        Fmt.(list ~sep:(unit " -> ") (pair ~sep:(unit " : ") string string))
        functors;
      Fmt.pr "packages %a\n"
        Fmt.(list ~sep:(unit "@;") pp_package) packages;
      let filename =
        let fn = List.hd (List.rev (String.split_on_char '/' Sys.argv.(1))) in
        String.sub fn 0 (String.index fn '.')
      in
      output_config filename job_name args functors packages)
    jobs

  (* output_configs jobs *)

(*
  ;Fmt.pr "%a"
      Ocaml_common.Printast.implementation original ;
*)
  ; `Ok ()

open Cmdliner

let setup_log =
  Term.(const (fun style_renderer level ->
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())
    )
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let unikernel_file =
  Arg.value
  @@ Arg.(opt file) "unikernel.ml"
  @@ Arg.info ["unikernel"]

let default_cmd =
  let doc = "" in
  let man = [
    `P {|
  TODO our incredibly helpful documentation.
  |}
  ] in
  Term.(ret (const cmd_everything $ setup_log $ unikernel_file)),
  Term.info "noinfo" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval default_cmd with
  | `Ok _ -> exit 0 | _ -> exit 1
