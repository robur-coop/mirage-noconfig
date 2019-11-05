open Rresult
open Astring

(* sample opam entry to add:
x-mirage-device: [
  [ # foo
    ["default" true]
    ["findlib" "foo.bar"]
    ["interface" "Mirage_kv.RO"]
    ["functor" "Mirage_kv_mem.Make"]
  ]]
*)

(* will parse into a mapping from Mirage_kv.RO:
# pkg_configs () |> R.get_ok|>Astring.String.Map.get_any_binding;;
".opam-switch/packages/samplepkg.~dev/opam": x-mirage-device
- : string * device_impl list =
("Mirage_kv.RO",
 [{is_default = true; interface_sig = "Mirage_kv.RO";
   interface_functor = "Mirage_kv_mem.Make";
   findlib_pkgs = ["mirage-kv-mem"]; description = ""}])
*)

module OPT = OpamParserTypes

type device_sig = {
  sig_interface: string ;
  sig_findlib: string list;
  sig_description:string ;
}
let empty_sig = {
  sig_interface = "" ;
  sig_findlib = [] ;
  sig_description = "";
}

type device_impl =
  { is_default : bool ;
    interface_sig : string ; (* foreign key to device_sig.interface*)
    interface_functor: string ;
    findlib_pkgs : string list ;
    description: string ;
  }
let empty_impl = {
  is_default = false;
  interface_sig = "" ;
  interface_functor = "";
  findlib_pkgs = [];
  description = "";
}
module DevMap = struct
  include String.Map
  let add_implementations m sig_name implementations =
    match find_opt sig_name m with
    | None -> add sig_name (implementations:device_impl list) m
    | Some existing ->
      (* TODO should be a Set *)
      add sig_name (implementations @ existing) m
end

let parse_file fn =
  begin match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | None ->
      R.error_msg
        "Environment variable OPAM_SWITCH_PREFIX not set, \
         maybe you need to run this command before invoking: \
         eval `opam config env`"
    | Some prefix -> Ok prefix
  end >>= fun prefix ->
  let full_path = prefix ^ "/" ^ fn in
  match OpamParser.file full_path with
  | exception _ ->
    R.error_msgf "parsing opam file failed: %S" full_path
  | f -> Ok f.file_contents

let pkg_names () =
  parse_file ".opam-switch/switch-state" >>|
  List.fold_left (fun acc -> function
      | OPT.Variable (_loc1, "roots", OPT.List (_loc2, vals)) ->
        List.fold_left (fun acc -> function
            | OPT.String (_, pkg_name) -> pkg_name::acc
            | _ -> (*ignore non-string entries: *) acc
          ) [] vals
      | _ -> acc
    ) []

let map_str_kv (type acc) entry name
    (f:string -> acc -> (acc, 'e) result) (acc:acc) =
  match entry with
  | OPT.List (_loc, [
      OPT.String (_, k) ;
      OPT.String (_, v) ;
    ]) when String.equal k name -> f v acc
  | _ -> Ok acc

let parse_mirage_sig vallist =
  match vallist with
  | OPT.List (_loc, sig_list) ->
    (* fold over list of signatures*)
    List.fold_left
      (fun acc -> function
         | OPT.List (_loc, sig_list) ->
           acc >>= fun acc ->
           (* parse signature key-values *)
           List.fold_left
             (fun (acc:(device_sig, 'a) result) kv ->
                acc >>= map_str_kv kv "interface"
                  (fun sig_interface acc ->
                     R.ok {acc with sig_interface})
                >>= map_str_kv kv "description"
                  (fun sig_description acc ->
                     R.ok {acc with sig_description})
                >>= fun acc ->
                match kv with
                | OPT.List (_, (OPT.String (_, "findlib")::pkgs)) ->
                  let sig_findlib = List.fold_left
                      (fun acc -> function
                         | OPT.String (_, pkg) -> pkg::acc
                         | _ -> assert false
                      )
                      [] pkgs in
                  R.ok {acc with sig_findlib }
                | _ -> R.ok acc (*R.error_msg "signature entry is not a list"*)
             ) (R.ok empty_sig) sig_list
           >>= fun new_device_sig ->
           R.ok (new_device_sig :: acc)
         | _ -> R.error_msg "signature is not a list"
      )
      (R.ok []) sig_list
  | _ -> R.error_msg "device signature list is not a list"

let parse_mirage_device _orig_map vallist =
  match vallist with
  | OPT.List (_loc, dev_lst) ->
    (* fold over each device impl description: *)
    List.fold_left
      (fun acc -> function
         | OPT.List (_loc, kvs) ->
           (* fold over each kv pair for a given device*)
           acc >>= fun acc ->
           let this =
             List.fold_left (fun acc -> function
                 | OPT.List (_, [
                     OPT.String (_, "description") ;
                     OPT.String (_, description)
                   ] ) -> {acc with description }
                 | OPT.List (_, [
                     OPT.String (_, "interface") ;
                     OPT.String (_, interface_sig)
                   ] ) -> {acc with interface_sig }

                 | OPT.List (_, [
                     OPT.String (_, "functor") ;
                     OPT.String (_, interface_functor)
                   ] ) -> {acc with interface_functor }
                 | OPT.List (_, [
                     OPT.String (_, "default") ;
                     OPT.Bool (_, is_default)
                   ] ) -> {acc with is_default }
                 | OPT.List (_, ((OPT.String (_, "findlib"))::values)) ->
                   let values = List.map (function
                       | OPT.String (_, x) -> x
                       | _ -> assert false) values in
                   {acc with findlib_pkgs = values }
                 | _ -> acc
               ) empty_impl kvs
           in
           Ok (this :: acc)
         | _ -> acc
      ) (Ok []) dev_lst
  | _ -> R.error_msgf "x-mirage-device is not a list"

let pkg_configs () =
  pkg_names () >>=
  List.fold_left (fun m pkgname ->
      m >>= fun m ->
      let fn = ".opam-switch/packages/" ^ pkgname ^ "/opam" in
      parse_file fn >>= fun conf ->
      let relevant = List.fold_left (fun acc -> function
          | OPT.Variable (_loc, "x-mirage-device", vallist) ->
            Fmt.epr "%S: x-mirage-device\n%!" fn;
            acc >>= fun acc ->
            parse_mirage_device acc vallist >>| fun impls ->
            List.fold_left (fun acc impl ->
                DevMap.add_implementations acc impl.interface_sig [impl]
              ) acc impls
          | OPT.Variable (_loc, "x-mirage-interface", vallist) ->
            acc >>= fun acc ->
            parse_mirage_sig vallist >>| fun _impl ->
            (* TODO currently ignored *)
            acc
          | _ -> acc
        ) (Ok m) conf in
      relevant
    )
    (Ok DevMap.empty)
