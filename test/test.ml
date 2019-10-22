
let test_job =
  let module M = struct
    type t = string * string list * (string * string) list
    let pp ppf (name, args, funs) =
      Fmt.pf ppf "module %s, start arguments %a, functors %a"
        name Fmt.(list ~sep:(unit " ") string) args
        Fmt.(list ~sep:(unit " ") (parens (pair ~sep:(unit " : ") string string)))
        funs
    let equal (n, a, f) (n', a', f') =
      String.equal n n' && List.for_all2 String.equal a a' &&
      List.for_all2 (fun (n, t) (n', t') -> String.equal n n' && String.equal t t')
        f f'
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

let base_unikernel = {|
module Main (T : Mirage_time_lwt.S) = struct
  let start _ = Lwt.return_unit
end
|}

let basic () =
  let lexbuf = Lexing.from_string base_unikernel in
  let _, jobs, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check int "no package" 0 (List.length packages));
  Alcotest.(check (list test_job) "job is expected one"
              [ ("Main", ["_"], [ "T", "Mirage_time_lwt.S" ]) ] jobs)

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

let tests = [
  "Parser", [
    "basic unikernel", `Quick, basic ;
    "with a package", `Quick, with_package ;
    "with two packages", `Quick, with_packages ;
    "with a package with a lower bound", `Quick, with_package_lower_bound ;
    "with a package with an upper bound", `Quick, with_package_upper_bound ;
    "with a package with a bounds", `Quick, with_package_bounds ;
    "with many packages", `Quick, with_many_packages ;
  ]
]

let () = Alcotest.run "noconfig tests" tests
