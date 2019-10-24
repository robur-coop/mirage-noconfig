open Astring

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
  | Lident plain -> String.concat ~sep:"." @@ plain :: acc
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
  List.filter (String.is_prefix ~affix:"Mirage_") elems

type device =
  [ `Posix_clock | `Monotonic_clock | `Random | `Time
  | `Console
  | `Block | `Kv_ro | `Kv_rw
  | `Network | `Ethernet | `Arp | `Ipv4 | `Stackv4 | `Ipv6
  | `Resolver
  | `Conduit
  | `Http ]

let compare_device a b =
  match a, b with
  (* order is dependency order ;) *)
  | `Posix_clock, `Posix_clock -> 0 | `Posix_clock , _ -> -1 | _, `Posix_clock -> 1
  | `Monotonic_clock, `Monotonic_clock -> 0 | `Monotonic_clock, _ -> -1 | _, `Monotonic_clock -> 1
  | `Random, `Random -> 0 | `Random, _ -> -1 | _, `Random -> 1
  | `Time, `Time -> 0 | `Time, _ -> -1 | _, `Time -> 1
  | `Console, `Console -> 0 | `Console, _ -> -1 | _, `Console -> 1
  | `Block, `Block -> 0 | `Block, _ -> -1 | _, `Block -> 1
  | `Kv_ro, `Kv_ro -> 0 | `Kv_ro, _ -> -1 | _, `Kv_ro -> 1
  | `Kv_rw, `Kv_rw -> 0 | `Kv_rw, _ -> -1 | _, `Kv_rw -> 1
  | `Network, `Network -> 0 | `Network, _ -> -1 | _, `Network -> 1
  | `Ethernet, `Ethernet -> 0 | `Ethernet, _ -> -1 | _, `Ethernet -> 1
  | `Arp, `Arp -> 0 | `Arp, _ -> -1 | _, `Arp -> 1
  | `Ipv4, `Ipv4 -> 0 | `Ipv4, _ -> -1 | _, `Ipv4 -> 1
  | `Stackv4, `Stackv4 -> 0 | `Stackv4, _ -> -1 | _, `Stackv4 -> 1
  | `Ipv6, `Ipv6 -> 0 | `Ipv6, _ -> -1 | _, `Ipv6 -> 1
  | `Resolver, `Resolver -> 0 | `Resolver, _ -> -1 | _, `Resolver -> 1
  | `Conduit, `Conduit -> 0 | `Conduit, _ -> -1 | _, `Conduit -> 1
  | `Http, `Http -> 0 (* | `Http, _ -> -1 | _, `Http -> 1 *)

let device = function
  | "PCLOCK" -> `Posix_clock
  | "MCLOCK" | "Mirage_clock.MCLOCK" -> `Monotonic_clock
  | "RANDOM" | "Mirage_random.C" -> `Random
  | "TIME" | "Mirage_time_lwt.S" -> `Time
  | "CONSOLE" -> `Console
  | "BLOCK" -> `Block
  | "KV_RO" | "Mirage_kv_lwt.RO" -> `Kv_ro
  | "KV_RW" | "Mirage_kv_lwt.RW" -> `Kv_rw
  | "NETWORK" | "Mirage_net_lwt.S" | "Mirage_net.S" -> `Network
  | "ETHERNET" | "Mirage_ethernet.S" -> `Ethernet
  | "ARP" -> `Arp
  | "IPV4" -> `Ipv4
  | "STACKV4" | "Mirage_stack_lwt.V4" | "Mirage_stack.V4" -> `Stackv4
  | "IPV6" -> `Ipv6
  | "Resolver_lwt.S" -> `Resolver
  | "Conduit_mirage.S" -> `Conduit
  | "HTTP" -> `Http
  | x ->
    Logs.warn (fun m -> m "unknown device %s" x);
    assert false

module DM = Map.Make(struct type t = device let compare = compare_device end)

let map_foreign = function
  | `Posix_clock -> "pclock"
  | `Monotonic_clock -> "mclock"
  | `Random -> "random"
  | `Time -> "time"
  | `Console -> "console"
  | `Block -> "block"
  | `Kv_ro -> "kv_ro"
  | `Kv_rw -> "kv_rw"
  | `Network -> "network"
  | `Ethernet -> "ethernet"
  | `Arp -> "arpv4"
  | `Ipv4 -> "ipv4"
  | `Stackv4 -> "stackv4"
  | `Ipv6 -> "ipv6"
  | `Resolver -> "resolver"
  | `Conduit -> "conduit"
  | `Http -> "http"

(* this is already in mirage, but not accessible, so write it again here *)
let dependencies = function
  | `Posix_clock | `Monotonic_clock | `Random | `Time | `Console -> []
  | `Block -> []
  | `Kv_ro | `Kv_rw -> []
  | `Network -> []
  | `Ethernet -> [ `Network ]
  | `Arp -> [ `Ethernet ]
  | `Ipv4 -> [ `Ethernet ; `Arp ]
  | `Stackv4 -> [ `Network ]
  | `Ipv6 -> [ `Ethernet ] (* + configuration -- but that should be optional / provided via key (similar to v4) *)
  | `Resolver -> [ `Stackv4 ]
  | `Conduit -> [ `Stackv4 ]
  | `Http -> [ `Conduit ]

(* some environment to match up the (already present/configured) devices *)
(* what is the access pattern here?
   - distinguish between "single" device and multiple
   --> single one is straightforward (register name, use that name)
   - multiple ones, their use sites:
   -- lookup (device, name) -> use the result

   - some examples:
      (n : network) (e : ethernet) (a : arp) (i : ipv4)
     --> all single
      (e : ethernet) (a : arp) (i : ipv4) (n : network)
     --> all single, order is not relevant

      (a : ethernet) (b : ethernet)
     --> should lead to (netif_a, netif_b), etif netif_a, etif netif_b

      (a_eth : ethernet) (b_eth : ethernet) (a_arp : arp) (b_arp : arp)
     --> (netif_a, netif_b), etif_a = etif netif_a, etif_b = etif netif_b, arp etif_a, arp etif_b
*)

(* idempotent *)
let real_name name =
  (match Astring.String.cut ~sep:"_" name with
   | None -> name
   | Some (a, _) -> a)
  |> String.Ascii.lowercase

let binding_name name dev =
  Printf.sprintf "%s_%s" (real_name name) (map_foreign dev)

let expr name dev = match dev with
  | `Posix_clock -> "default_posix_clock"
  | `Monotonic_clock -> "default_monotonic_clock"
  | `Time -> "default_time"
  | `Random -> "default_random"
  | `Console -> "default_console"
  | `Block -> Printf.sprintf "block_of_file %S" "disk.img" (* TODO: if target == solo5 'storage' else 'disk.img' *)
  | `Kv_ro -> Printf.sprintf "generic_kv_ro %S" name (* TODO key: let name_key = Key.(value @@ kv_ro ~group:"$name" ()) *)
  | `Kv_rw -> Printf.sprintf "generic_kv_rw %S" name (* key as above *)
  | `Network -> "default_network"
  | `Ethernet -> "etif"
  | `Arp -> "arp"
  | `Ipv4 -> "create_ipv4"
  | `Stackv4 -> "generic_stackv4"
  | `Ipv6 -> "create_ipv6 (etif default_network) { addresses = [] ; netmasks = [] ; gateways = [] }" (* TODO ??? *)
  | `Resolver -> "resolver_dns"
  | `Conduit -> "conduit_direct ~tls:true"
  | `Http -> "cohttp_server"

let init name dev =
  match dev with
  | `Network -> Printf.sprintf "netif ~group:%S %S" name name (* for xen use numbers instead of name for now *)
  | `Ipv4 -> Printf.sprintf "create_ipv4 ~group:%S" name
  | `Stackv4 -> Printf.sprintf "generic_stackv4 ~group:%S" name
  | _ -> expr name dev

let initialisation_code names devices =
  (* return is a list of pairs of strings (binding, expr), it will result in:
     let $binding = $expr in *)
  let add_extend map v dev =
    let v' = match DM.find_opt dev map with
      | None -> [ v ]
      | Some xs -> v :: xs
    in
    DM.add dev v' map
  in
  let used = List.fold_left2 add_extend DM.empty names devices in
  (* we now know all explicitly used devices in a map dev -> [ name1; name2 ] *)
  (* let's add to that the dependencies of each device *)
  DM.fold (fun key names acc ->
      let rec arg_deps ?(multiple = false) key name acc =
        match dependencies key with
        | [] ->
          (* no dependencies, we add the empty argument vector! *)
          (name, key), add_extend acc (name, []) key
        | deps ->
          (* we have (some) dependencies, and need to find the arguments *)
          let args, acc' =
            List.fold_left (fun (args, acc) dep ->
                match DM.find_opt dep acc with
                | None ->
                  (* create, and also need to check deps of dep *)
                  let arg, acc' = arg_deps dep name acc in
                  arg :: args, acc'
                | Some [] -> assert false (* this is safe, we always insert non-empty lists *)
                | Some [ (one, _) ] when not multiple ->
                  (* if single, use the existing one (potentially renaming)! *)
                  (one, dep) :: args, acc
                | Some [ (one, _) ] when multiple ->
                  (* multiple: use if name matches, otherwise create another *)
                  if String.equal name one then
                    (one, dep) :: args, acc
                  else
                    let arg, acc' = arg_deps ~multiple dep name acc in
                    arg :: args, acc'
                | Some xs when not multiple ->
                  (* single: assume its around or pick first *)
                  if List.mem name (List.map fst xs) then
                    (name, dep) :: args, acc
                  else
                    (fst (List.hd xs), dep) :: args, acc
                | Some xs (* when multiple -- enabling guard leads to incomplete pattern match *) ->
                  if List.mem name (List.map fst xs) then
                    (name, dep) :: args, acc
                  else
                    let arg, acc' = arg_deps ~multiple dep name acc in
                    arg :: args, acc')
              ([], acc) deps
          in
          (name, key), add_extend acc' (name, List.rev args) key
      in
      match names with
      | [ name ] -> snd (arg_deps key name acc)
      | names ->
        (* we ended up with multiple devices, and need to create multiple
           other dependent devices *)
        List.fold_left (fun acc name ->
            let _, acc' = arg_deps ~multiple:true key name acc in
            acc') acc names)
    used DM.empty

let output_initialisation out =
  (* output is a map "device" (name, args) list with args being (dev, name) list *)
  let args_str xs =
    match xs with
    | [] -> ""
    | xs ->
      let names =
        let names, devs = List.split xs in
        List.map2 binding_name names devs
      in
      String.concat ~sep:" " ("" :: names)
  in
  DM.fold (fun key names acc ->
      match names with
      | [ (x, args) ] -> (binding_name x key, expr x key ^ args_str args) :: acc
      | xs ->
        let names = List.map (fun (b, _) -> binding_name b key) xs
        and exprs = List.map (fun (b, args) -> init b key ^ args_str args) xs
        in
        List.combine names exprs @ acc
    ) out [] |> List.rev

let pp_package ppf (name, min, max) =
  Fmt.pf ppf "package %a %a %S"
    Fmt.(option ~none:(unit "") (prefix (unit "~min:") (quote string))) min
    Fmt.(option ~none:(unit "") (prefix (unit "~max:") (quote string))) max
    name

let output_config fmt unikernel filename modname _args functors packages =
  (* assert (List.length functors = List.length args);
     -- this is violated by e.g. app_info atm*)
  let fnames, ftypes = List.split functors in
  let devices = List.map device ftypes in
  Fmt.pf fmt "open Mirage@.";
  Fmt.pf fmt "let packages = @[[ %a ]@]@.@."
    Fmt.(list ~sep:(unit "; ") pp_package) packages;
  Fmt.pf fmt "let main = foreign ~packages %S @[(%a @-> job)@]@.@."
    (String.Ascii.capitalize (filename ^ "." ^ modname))
    Fmt.(list ~sep:(unit " @-> ") string)
    (List.map map_foreign devices);
  let init = initialisation_code (List.map real_name fnames) devices in
  Fmt.pf fmt "let () =@.@[%a@] in@.register %S @[[ main $ %a ]@]@."
    Fmt.(list ~sep:(unit " in@.") (prefix (unit "let ") (pair ~sep:(unit " = ") string string)))
    (output_initialisation init)
    unikernel Fmt.(list ~sep:(unit " $ ") string)
    (List.map2 binding_name fnames devices)

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
