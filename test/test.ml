
let test_job =
  let module M = struct
    type t = string * string list * Libnoconfig.funktor list
    let pp ppf (name, args, funs) =
      Fmt.pf ppf "module %s, start arguments %a, functors %a"
        name Fmt.(list ~sep:(unit " ") string) args
        Fmt.(list ~sep:(unit " ") Libnoconfig.pp_funktor)
        funs
    let equal (n, a, f) (n', a', f') =
      String.equal n n' &&
      List.length a = List.length a' &&
      List.for_all2 String.equal a a' &&
      List.length f = List.length f' &&
      List.for_all2 Libnoconfig.eq_funktor f f'
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let test_funktors =
  let module M = struct
    type t = Libnoconfig.funktor list
    let pp ppf funs =
      Fmt.(list ~sep:(unit " ") Libnoconfig.pp_funktor) ppf funs
    let equal f f' =
      List.length f = List.length f' &&
      List.for_all2 Libnoconfig.eq_funktor f f'
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let opt_eq f a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> f a b
  | _ -> false

let test_package =
  let module M = struct
    type t = string * string option * string option
    let pp ppf (name, min, max) =
      Fmt.pf ppf "%S (min: %a max: %a)"
        name Fmt.(option ~none:(unit "no") string) min
        Fmt.(option ~none:(unit "no") string) max
    let equal (n, mi, ma) (n', mi', ma') =
      String.equal n n' && opt_eq String.equal mi mi' &&
      opt_eq String.equal ma ma'
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let test_output =
  let module M = struct
    type t = (string * (string * Libnoconfig.device) list) list Libnoconfig.DM.t
    let pp ppf out =
      let pp_args ppf args =
        let names, devs = List.split args in
        Fmt.(list ~sep:(unit " ") string) ppf
          (List.map2 Libnoconfig.binding_name names devs)
      in
      let pp_one key ppf (name, args) =
        Fmt.pf ppf "%s = ?? %a" (Libnoconfig.binding_name name key) pp_args args
      in
      Libnoconfig.DM.iter (fun k v ->
          Fmt.(list ~sep:(unit "@.") (pp_one k)) ppf v)
        out
    let equal a b =
      Libnoconfig.DM.equal (fun a b ->
          List.length a = List.length b &&
          let n, a = List.split a and n', a' = List.split b in
          List.for_all2 String.equal n n' &&
          List.for_all2 (fun a a' ->
              List.length a = List.length a' &&
              let n, a = List.split a and n', a' = List.split a' in
              List.for_all2 String.equal n n' &&
              List.for_all2
                (fun a b -> Libnoconfig.compare_device a b = 0) a a') a a')
        a b
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let base_unikernel = {|
module Main (T : Mirage_time_lwt.S) = struct
  let start _ = Lwt.return_unit
end
|}

let f = {
  Libnoconfig.name = "T" ;
  typ = "Mirage_time_lwt.S" ;
  lower_bound = None ;
  upper_bound = None ;
}

let basic () =
  let lexbuf = Lexing.from_string base_unikernel in
  let _, jobs, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check int "no package" 0 (List.length packages));
  Alcotest.(check (list test_job) "job is expected one"
              [ ("Main", ["_"], [ f ]) ] jobs)

let with_package () =
  let package = {_|[@@@package "foo"]|_} in
  let lexbuf = Lexing.from_string (package ^ base_unikernel) in
  let _, _, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check (list test_package) "one package"
              [ "foo", None, None ] packages)

let with_packages () =
  let package = {_|
[@@@package "foo"]
[@@@package "bar"]
|_} in
  let lexbuf = Lexing.from_string (package ^ base_unikernel) in
  let _, _, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check (list test_package) "two packages"
              [ "foo", None, None ; "bar", None, None ] packages)

let with_package_lower_bound () =
  let package = {_|[@@@package "foo" {|>= "1.2"|}]|_} in
  let lexbuf = Lexing.from_string (package ^ base_unikernel) in
  let _, _, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check (list test_package) "one package"
              [ "foo", Some "1.2", None ] packages)

let with_package_upper_bound () =
  let package = {_|[@@@package "foo" {|< "2.0"|}]|_} in
  let lexbuf = Lexing.from_string (package ^ base_unikernel) in
  let _, _, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check (list test_package) "one package"
              [ "foo", None, Some "2.0" ] packages)

let with_package_bounds () =
  let package = {_|[@@@package "foo" {|>= "1.0" & < "2.0"|}]|_} in
  let lexbuf = Lexing.from_string (package ^ base_unikernel) in
  let _, _, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check (list test_package) "one package"
              [ "foo", Some "1.0", Some "2.0" ] packages)

let with_many_packages () =
  let package = {_|
[@@@package "foo" {|>= "1.0"|}]
[@@@package "bar"]
[@@@package "baz" {|< "4.0"|}]
[@@@package "foobar" {|>= "1.0" & < "3.4"|}]
|_} in
  let lexbuf = Lexing.from_string (package ^ base_unikernel) in
  let _, _, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check (list test_package) "many packages"
              [ "foo", Some "1.0", None ;
                "bar", None, None ;
                "baz", None, Some "4.0" ;
                "foobar", Some "1.0", Some "3.4"
              ] packages)

let towards_output s =
  let lexbuf = Lexing.from_string s in
  let _, jobs, _ = Libnoconfig.parse lexbuf in
  let _, _, functors = List.hd jobs in
  let names, types =
    List.split (List.map (fun f -> f.Libnoconfig.name, f.typ) functors)
  in
  let types' = List.map Libnoconfig.device types in
  let names' = List.map Libnoconfig.real_name names in
  names', types'

let output_basic () =
  let names, devices = towards_output base_unikernel in
  Alcotest.(check test_output "basic output test"
              (Libnoconfig.DM.singleton `Time [ "t", [] ])
              (Libnoconfig.initialisation_code names devices))

let output_deps () =
  let unikernel = {|
module Main (E : Mirage_ethernet.S) = struct
  let start _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  Alcotest.(check test_output "output test with dependency"
              (Libnoconfig.DM.add `Network [ "e", [] ]
                 (Libnoconfig.DM.singleton `Ethernet [ "e", [ "e", `Network ]]))
              (Libnoconfig.initialisation_code names devices))

let output_deps_rename () =
  let unikernel = {|
module Main (E : Mirage_ethernet.S) (N : Mirage_net.S) = struct
  let start _ _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  Alcotest.(check test_output "output test with dependency, renaming"
              (Libnoconfig.DM.add `Network [ "n", [] ]
                 (Libnoconfig.DM.singleton `Ethernet [ "e", [ "n", `Network ]]))
              (Libnoconfig.initialisation_code names devices))

let output_rec_deps () =
  let unikernel = {|
module Main (R : Resolver_lwt.S) = struct
  let start _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  Alcotest.(check test_output "output test with recursive dependency"
              (Libnoconfig.DM.add `Network [ "r", [] ]
                 (Libnoconfig.DM.add `Stackv4 [ "r", [ "r", `Network ] ]
                    (Libnoconfig.DM.singleton `Resolver [ "r", [ "r", `Stackv4 ]])))
              (Libnoconfig.initialisation_code names devices))

let output_multi_devices () =
  let unikernel = {|
module Main (S : Mirage_net.S) (T : Mirage_net.S) = struct
  let start _ _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  Alcotest.(check test_output "output test with multiple devices"
              (Libnoconfig.DM.singleton `Network [ "s", [] ; "t", [] ])
              (Libnoconfig.initialisation_code names devices))

let output_multi_devices_with_deps () =
  let unikernel = {|
module Main (S : Mirage_stack.V4) (T : Mirage_stack.V4) = struct
  let start _ _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  Alcotest.(check test_output "output test with multiple devices and dependencies"
              (Libnoconfig.DM.add `Network [ "s", [] ; "t", [] ]
                 (Libnoconfig.DM.singleton `Stackv4
                    [ "s", [ "s", `Network ] ; "t", [ "t", `Network ]]))
              (Libnoconfig.initialisation_code names devices))

let output_ordered () =
  let unikernel = {|
module Main (N : NETWORK) (E : ETHERNET) (A : ARP) (I : IPV4) = struct
  let start _ _ _ _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  let dm =
    Libnoconfig.DM.add `Network [ "n", [] ]
      (Libnoconfig.DM.add `Ethernet [ "e", [ "n", `Network ]]
         (Libnoconfig.DM.add `Arp [ "a", [ "e", `Ethernet ]]
            (Libnoconfig.DM.singleton `Ipv4
               [ "i", [ "e", `Ethernet ; "a", `Arp ]])))
  in
  Alcotest.(check test_output "output test ordered deps" dm
              (Libnoconfig.initialisation_code names devices));
  let unikernel' = {|
module Main (E : ETHERNET) (A : ARP) (I : IPV4) (N : NETWORK) = struct
  let start _ _ _ _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel' in
  Alcotest.(check test_output "output test ordered deps" dm
              (Libnoconfig.initialisation_code names devices))

let output_multi_names () =
  let unikernel = {|
module Main (A_eth : ETHERNET) (B_eth : ETHERNET) (A_arp : ARP) (B_arp : ARP) = struct
  let start _ _ _ _ = Lwt.return_unit
end
|} in
  let names, devices = towards_output unikernel in
  Alcotest.(check test_output "output test ordered deps"
              (Libnoconfig.DM.add `Network [ "a", [] ; "b", [] ]
                 (Libnoconfig.DM.add `Ethernet [ "a", [ "a", `Network ]; "b", [ "b", `Network ]]
                    (Libnoconfig.DM.singleton `Arp [ "a", [ "a", `Ethernet ]; "b", [ "b", `Ethernet ]])))
              (Libnoconfig.initialisation_code names devices))

let get_functors s =
  let lexbuf = Lexing.from_string s in
  let _, jobs, _ = Libnoconfig.parse lexbuf in
  let _, _, functors = List.hd jobs in
  functors

let f = { f with typ = "Mirage_time.S" }

let basic_functor_lower_bound () =
  let unikernel = {_|
module Main (T : Mirage_time.S[@package {|>= "1.0"|}]) = struct
  let start _ = Lwt.return_unit
end
|_} in
  Alcotest.check test_funktors "functor test with lower bound"
    [ { f with lower_bound = Some "1.0" } ]
    (get_functors unikernel)

let basic_functor_upper_bound () =
  let unikernel = {_|
module Main (T : Mirage_time.S[@package {|< "1.0"|}]) = struct
  let start _ = Lwt.return_unit
end
|_} in
  Alcotest.check test_funktors "functor test with upper bound"
    [ { f with upper_bound = Some "1.0" } ]
    (get_functors unikernel)

let basic_functor_bounds () =
  let unikernel = {_|
module Main (T : Mirage_time.S[@package {|>= "1.0" & < "3.0"|}]) = struct
  let start _ = Lwt.return_unit
end
|_} in
  Alcotest.check test_funktors "functor test with bounds"
    [ { f with lower_bound = Some "1.0" ; upper_bound = Some "3.0" } ]
    (get_functors unikernel)

let basic_functors_bounds () =
  let unikernel = {_|
module Main (T : Mirage_time.S[@package {|>= "1.0" & < "3.0"|}]) (T2 : Mirage_time.S[@package {|>= "2.0"|}]) (T3 : Mirage_time.S[@package {|< "4.0"|}]) (T4 : Mirage_time.S) (T5 : Mirage_time.S[@package {|>= "1.0" & < "4.0"|}]) = struct
  let start _ = Lwt.return_unit
end
|_} in
  Alcotest.check test_funktors "functors test with bounds"
    [
      { f with lower_bound = Some "1.0" ; upper_bound = Some "3.0" } ;
      { f with name = "T2" ; lower_bound = Some "2.0" } ;
      { f with name = "T3" ; upper_bound = Some "4.0" } ;
      { f with name = "T4" } ;
      { f with name = "T5" ; lower_bound = Some "1.0" ; upper_bound = Some "4.0" } ;
    ]
    (get_functors unikernel)

let tests = [
  "Package parser", [
    "basic unikernel", `Quick, basic ;
    "with a package", `Quick, with_package ;
    "with two packages", `Quick, with_packages ;
    "with a package with a lower bound", `Quick, with_package_lower_bound ;
    "with a package with an upper bound", `Quick, with_package_upper_bound ;
    "with a package with a bounds", `Quick, with_package_bounds ;
    "with many packages", `Quick, with_many_packages ;
  ] ;
  "Functor parser", [
    "basic output", `Quick, output_basic ;
    "dependency output", `Quick, output_deps ;
    "renamed dependency output", `Quick, output_deps_rename ;
    "recursive dependency output", `Quick, output_rec_deps ;
    "multiple devices", `Quick, output_multi_devices ;
    "multiple devices with deps", `Quick, output_multi_devices_with_deps ;
    "ordered output", `Quick, output_ordered ;
    "output multiple names", `Quick, output_multi_names ;
  ] ;
  "Functor with bounds parser", [
    "basic functor with lower bound", `Quick, basic_functor_lower_bound ;
    "basic functor with upper bound", `Quick, basic_functor_upper_bound ;
    "basic functor with bounds", `Quick, basic_functor_bounds ;
    "basic functors with bounds", `Quick, basic_functors_bounds ;
  ]
]

let () = Alcotest.run "noconfig tests" tests
