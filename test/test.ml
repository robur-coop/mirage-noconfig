
let basic () =
  let lexbuf = Lexing.from_string {|
      module Main (T : Mirage_time_lwt.S) = struct
        let start _ = Lwt.return_unit
      end
  |} in
  let _, jobs, packages = Libnoconfig.parse lexbuf in
  Alcotest.(check int "no package" 0 (List.length packages));
  Alcotest.(check int "one job" 1 (List.length jobs));
  match jobs with
  | [ (name, args, funs) ] ->
    Alcotest.(check string "name is Main" "Main" name);
    Alcotest.(check (list string) "arguments is _" [ "_" ] args);
    Alcotest.(check (list (pair string string)) "functor is [ T , time ]"
                [ "T", "Mirage_time_lwt.S" ] funs)
  | _ -> assert false

let tests = [
  "Parser", [ "basic unikernel", `Quick, basic ]
]

let () = Alcotest.run "noconfig tests" tests
