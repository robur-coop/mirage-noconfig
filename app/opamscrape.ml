open Rresult
open Astring

(* sample opam entry to add:
x-mirage-device: [
  [ # foo
    ["default" true]
    ["findlib" "foo.bar"]
    ["functor" "Sample"]
  ]
  [ # bar
    ["findlib" "bar" "baz"]
    ["functor" "Sample2"]
  ]
]
*)

(* will parse into:

[("samplepkg.~dev",
  [{is_default = false; interface_functor = "Sample2";
    findlib_pkgs = ["bar"; "baz"]; description = ""};
   {is_default = true; interface_functor = "Sample";
    findlib_pkgs = ["foo.bar"]; description = ""}])]

*)

module OPT = OpamParserTypes

type device_impl =
  { is_default : bool ;
    interface_functor: string ;
    findlib_pkgs : string list ;
    description: string ;
  }
let empty_impl = {
  is_default = false;
  interface_functor = "";
  findlib_pkgs = [];
  description = "";
}
module DevMap = String.Map

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
            parse_mirage_device acc vallist >>| fun impl ->
            DevMap.add pkgname impl acc
          | _ -> acc
        ) (Ok m) conf in
      relevant
    )
    (Ok DevMap.empty)
