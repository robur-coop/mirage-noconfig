(* Given a module name, [lookup name] looks for the findlib pkg
   that contains a native implementation of [name].cmxa
*)

open Astring

let cache = lazy (
  Findlib.init () ;
  Findlib.list_packages' ()
  |> List.fold_left (fun acc pkg ->
      try String.Map.add (
          Stdlib.String.capitalize_ascii @@
          Findlib.package_property ["native"] pkg "archive"
          |> Stdlib.String.split_on_char '.'
          |> List.hd (* get rid of cmxa ext *)
        ) pkg acc
      with _ -> acc)
    String.Map.empty
)

let lookup module_name =
  String.Map.find module_name (Lazy.force cache)


let find_undeclared_packages ~declared ~external_modules =
  (* Given a list of declared package dependencies and a list of
     module name, try to figure out which modules are not covered by a
     declared dependency.
  *)
  let declared = List.to_seq declared
          |> Seq.map (fun (pkg, _b, _b') -> pkg)
          |> String.Set.of_seq  in
  List.fold_left (fun (suggestions, missing) modname ->
      match lookup modname with
      | Some found when String.Set.mem found declared ->
        (* It's declared, and even installed. great! *)
        suggestions, missing

      | Some found ->
        (* It's NOT declared, but we believe we have found
         * the package, so now we can suggest it. *)
        (modname, found)::suggestions, missing

      | None ->
        (* We can't find it with Findlib, which means we can't check
         * if it was declared or not. *)
        suggestions, modname::missing
    ) ([],[]) external_modules
