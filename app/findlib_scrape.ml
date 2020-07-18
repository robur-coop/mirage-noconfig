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
