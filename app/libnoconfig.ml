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

let parse_bound bound =
  let open OpamParserTypes in
  (* >= "1.0" OR < "2.0" OR >= "1.0" & < "2.0" *)
  match OpamParser.value_from_string bound "" with
  | Prefix_relop (_pos, `Geq, String (_pos', data)) -> Some data, None
  | Prefix_relop (_pos, `Lt, String (_pos', data)) -> None, Some data
  | Logop (_pos, `And,
           (Prefix_relop (_pos', `Geq, String (_pos'', min))),
           (Prefix_relop (_pos''', `Lt, String (_pos'''', max)))) ->
    Some min, Some max
  | _ -> Logs.warn (fun m -> m "couldn't parse bound %s" bound); None, None

let extract_package payload =
  (* payload being: "foo" OR "foo" {|bound|}
     where bound may be any of the three forms:
     >= "1.0" OR < "2.0" OR >= "1.0" & < "2.0" *)
  let open Parsetree in
  match payload with
  | PStr [ { pstr_desc = Pstr_eval (e, _) ; _ } ] ->
    (* two kinds of e
       - Pexp_constant (Pconst_string ("foo", None))
       - Pexp_apply
           ({ pexp_desc = Pexp_constant (Pconst_string ("foo", None)); _ },
           [(_, { pexp_desc = Pexp_constant (Pconst_string ("> \"1.0.0\" ", _)) }) ]
    *)
    let name, min, max =
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (package_name, _)) -> package_name, None, None
      | Pexp_apply
          ({ pexp_desc = Pexp_constant (Pconst_string (package_name, _)) ; _ },
           [ _, { pexp_desc = Pexp_constant (Pconst_string (bound, _)) ; _ } ]) ->
        let min, max = parse_bound bound in
        package_name, min, max
      | _ -> assert false
    in
    (name, min, max)
  | _ -> assert false (* TODO proper error/warning *)

let collect_attributes things =
  let open Parsetree in
  List.fold_left (fun packages si ->
      match si.pstr_desc with
      | Pstr_attribute { attr_name ; attr_payload ; _ } ->
        begin match attr_name.Asttypes.txt with
          | "package" -> extract_package attr_payload :: packages
          | x ->
            Logs.warn (fun m -> m "skipping unknown attribute %s" x);
            packages
        end
      | _ -> packages)
    [] things |> List.rev

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

let device = function
  | "PCLOCK" -> `Posix_clock
  | "MCLOCK" -> `Monotonic_clock
  | "KV_RO" | "Mirage_kv_lwt.RO" -> `Kv_ro
  | "KV_RW" | "Mirage_kv_lwt.RW" -> `Kv_rw
  | "HTTP" -> `Http
  | "NETWORK" -> `Network
  | "ETHERNET" -> `Ethernet
  | "ARP" -> `Arp
  | "IPV4" -> `Ipv4
  | "TIME" | "Mirage_time_lwt.S" -> `Time
  | "STACKV4" | "Mirage_stack_lwt.V4" -> `Stackv4
  | "CONSOLE" -> `Console
  | "BLOCK" -> `Block
  | "Resolver_lwt.S" -> `Resolver
  | "Conduit_mirage.S" -> `Conduit
  | "IPV6" -> `Ipv6
  | "RANDOM" -> `Random
  | x ->
    Logs.warn (fun m -> m "unknown device %s" x);
    assert false

let map_foreign = function
  | `Posix_clock -> "pclock"
  | `Monotonic_clock -> "mclock"
  | `Kv_ro -> "kv_ro"
  | `Kv_rw -> "kv_rw"
  | `Http -> "http"
  | `Network -> "network"
  | `Ethernet -> "ethernet"
  | `Arp -> "arpv4"
  | `Ipv4 -> "ipv4"
  | `Time -> "time"
  | `Stackv4 -> "stackv4"
  | `Console -> "console"
  | `Block -> "block"
  | `Resolver -> "resolver"
  | `Conduit -> "conduit"
  | `Ipv6 -> "ipv6"
  | `Random -> "random"

(* and this is the actual hard part, need to keep some environment to construct proper terms *)
let map_register = function
  | _, `Posix_clock -> "default_posix_clock"
  | _, `Monotonic_clock -> "default_monotonic_clock"
  | _, `Time -> "default_time"
  | _, `Random -> "default_random"
  | _, `Console -> "default_console"
  | _, `Http -> "cohttp_server (conduit_direct ~tls:true (generic_stackv4 default_network))" (* TODO *)
  | name, `Kv_ro -> "generic_kv_ro " ^ String.lowercase_ascii name (* TODO *)
  | _, `Kv_rw -> "generic_kv_rw" (* TODO *)
  | _, `Block -> "block_of_file \"disk.img\"" (* TODO *)
  | _, `Network -> "default_network"
  | _, `Ethernet -> "etif default_network" (* TODO depending on (a) number of ethernet (b) whether network was used as well, reuse network binding *)
  | _, `Arp -> "arp (etif default_network)" (* TODO see above *)
  | _, `Ipv4 -> "create_ipv4 (etif default_network) (arp (etif default_network))" (* TODO same as above *)
  | _, `Ipv6 -> "create_ipv6 (etif default_network) { addresses = [] ; netmasks = [] ; gateways = [] }" (* TODO ??? *)
  | _, `Stackv4 -> "generic_stackv4 default_network" (* TODO *)
  | _, `Resolver -> "resolver_dns (generic_stack default_network)" (* TODO same as above*)
  | _, `Conduit -> "conduit_direct (generic_stack default_network)" (* TODO same as above *)
  | _ -> assert false

let pp_package ppf (name, min, max) =
  Fmt.pf ppf "package %a %a %S"
    Fmt.(option ~none:(unit "") (prefix (unit "~min:") (quote string))) min
    Fmt.(option ~none:(unit "") (prefix (unit "~max:") (quote string))) max
    name

let output_config fmt filename name _args functors packages =
  (* assert (List.length functors = List.length args);
     -- this is violated by e.g. app_info atm*)
  let fnames, ftypes = List.split functors in
  let devices = List.map device ftypes in
  Fmt.pf fmt "open Mirage@.";
  Fmt.pf fmt "let packages = @[[ %a ]@]@.@."
    Fmt.(list ~sep:(unit "; ") pp_package) packages;
  Fmt.pf fmt "let main = foreign ~packages %S @[(%a @-> job)@]@.@."
    (String.capitalize_ascii (filename ^ "." ^ name))
    Fmt.(list ~sep:(unit " @-> ") string)
    (List.map map_foreign devices);
  Fmt.pf fmt "let () = register %S @[[ main $ %a ]@]@."
    (String.lowercase_ascii name)
    Fmt.(list ~sep:(unit " $ ") string)
    (List.map map_register (List.combine fnames devices))

let parse lexbuf =
  let original, parsed =
    (* parse using the current compiler tooling,
       then convert to the {!PT} representation that this application
       knows about: *)
    let impl = Ocaml_common.Parse.implementation lexbuf in
    let module Cool = Migrate_parsetree.Convert
        (Migrate_parsetree.OCaml_current)(PT)
    in
    impl, Cool.copy_structure impl
  in
  (* topl_functors parsed ; *)
  (*  Fmt.pr "%a"
      Ocaml_common.Printast.implementation original ;*)
  let jobs = List.fold_left (fun acc expr ->
      match parse_job expr with
      | None -> acc
      | Some job -> job::acc
    ) [] parsed in
  let external_modules = ext_mirage_depend original in
  let packages = collect_attributes parsed in
  (external_modules, jobs, packages)
