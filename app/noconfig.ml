open Libnoconfig

let cmd_everything () _target_backend unikernel_file name output_file =
  let lexbuf = Lexing.from_channel (open_in_bin unikernel_file) in
  let external_modules, jobs, packages = parse lexbuf in
  Logs.debug (fun m -> m "\npackages from attributes: %a\n"
                 Fmt.(list ~sep:(unit" ; ") @@ fun ppf (n,c1,c2) ->
                      Fmt.pf ppf "%s %a %a" n
                        (option string) c1 (option string)c2) packages) ;
  Logs.debug (fun m -> m "\nExternal modules:\n%a\n----\n"
                 Fmt.(list ~sep:(unit"\n")string) external_modules) ;
  let () =
    let suggestions, missing = Findlib_scrape.find_undeclared_packages
        ~declared:packages ~external_modules in
    List.iter (fun modname -> Logs.warn (fun m ->
        m "Module %S is referenced, but cannot determine findlib package."
          modname)) missing ;
    (* TODO we should filter out the ones that Mirage
       automagically knows about, like mirage-clock? *)
    List.iter (fun (modname,findlibpkg) -> Logs.warn (fun m ->
        m "Module %S is referenced, unikernel file may need a %a declaration?"
          modname
          Fmt.(styled (`Fg (`Hi `Blue)) @@ (* double @@ escapes single @ below: *)
               Fmt.fmt "[@@@@@@package %S]") findlibpkg
      )) suggestions
  in
  let fmt = match output_file with
    | None -> Fmt.stdout
    | Some file -> Format.formatter_of_out_channel (Stdlib.open_out file)
  in
  List.iter (fun (job_name, args, functors) ->
      Logs.debug (fun m -> m "\n-- Job %S in %S:\n" job_name unikernel_file);
      let fs = List.map (fun f -> f.name, f.typ) functors in
      Logs.debug (fun m -> m "args: %a\nfunctors %a\n"
        Fmt.(list ~sep:(unit " ") string) args
        Fmt.(list ~sep:(unit " -> ") (pair ~sep:(unit " : ") string string))
        fs) ;
      Logs.debug (fun m -> m "packages %a\n"
        Fmt.(list ~sep:(unit "@;") pp_package) packages) ;
      let modulename = Filename.(chop_extension @@ basename unikernel_file) in
      let unikernel = match name with
        | Some "."
        | None -> Filename.(basename @@ dirname unikernel_file)
        | Some x -> x
      in
      output_config fmt unikernel modulename job_name args functors packages)
    jobs ;
  Ok ()

(* command-line utility *)

open Cmdliner

let setup_log =
  Term.(const (fun style_renderer level ->
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())
    )
        $ Fmt_cli.style_renderer ~docs:Manpage.s_common_options ()
        $ Logs_cli.level ~docs:Manpage.s_common_options ())

let file_or_path_conv : string Cmdliner.Arg.conv =
  (* an argument converter that takes either a path to a file,
     or to a directory (in which it will try to look for unikernel.ml)
  *)
  let normalize_path path =
    String.split_on_char '/' path
    |> List.fold_left (fun acc_tup elt -> match elt, acc_tup with
        | ("" | "."), (true,_) -> acc_tup
        | "..", (true, (("."|""|"..")::_ as acc)) -> true, ".."::acc
        | "..", (true, _::acc) -> true, acc (*if not special then remove*)
        | elt, (_, acc) -> true, elt::acc) (false,[])
    |> snd |> List.rev |> String.concat "/"
  in
  let pp ppf _x = Fmt.pf ppf "%S" "" in
  let check path =
    let path = if path = "" then "." else path in
    let path =
      if Sys.file_exists path && Sys.is_directory path
      then (if path = "." then Sys.getcwd () else path)
           ^ "/unikernel.ml" else path in
    let path = normalize_path path in
    (* file_exists returns true for directories as well: *)
    if Sys.file_exists path && not (Sys.is_directory path)
    then begin Logs.debug (fun m -> m "file %S" path); Ok path
    end else begin
      Error (`Msg (Format.sprintf
                     "Is this a unikernel? Unable to find %S" (normalize_path path)))
    end
  in
  Arg.conv (check,  pp)

let opt_converted (type v) (default:string) (conv:v Arg.conv) info =
  (* This is a wrapper around Cmdliner.Arg.opt that will
     return [conv default] instead of [default] when
     [use_default evaluated] is [true].
     [evaluated] is the result of the [{!Arg.value} {!Arg.opt}].
     (i.e. can be used to detect when option is not present on
     the commandline, or if the parsed value does not meet expectations).
     It feels like there should be an easier way to do this.
  *)
  Arg.value (Arg.opt (Arg.some conv) (None) info)
  |> Term.app (Term.const (function
      | None ->
        Arg.conv_parser conv default
      | Some explicit -> Ok explicit))
  |> Term.term_result

let unikernel_file =
  (* here we use {!opt_converted} to make sure we have a valid path*)
  opt_converted "." file_or_path_conv
  @@ Arg.info
    ~doc:{|Path to unikernel to analyze.
Defaults to 'unikernel.ml' in the current directory.|}
    ~docv:"FILE-OR-FOLDER" ["unikernel"]

let unikernel_name =
  let doc = "Unikernel name (defaults to dirname)" in
  Arg.(value & opt (some string) None & info [ "name" ] ~doc)

let output_file =
  let doc = "Filename of the generated config.ml (defaults to stdout)" in
  Arg.(value & opt (some string) None & info [ "output" ] ~doc ~docv:"FILE")

let target_backend =
  let backends = ["spt";"xen";"hvt";"unix";"qubes"] in
  (* TODO there are probably some more;
     if only mirage --help wasn't broken I could list them... *)
  Arg.opt (Arg.enum (List.map (fun x -> x,x) backends)) "unix"
  @@ Arg.info
    ~doc:("Target backend, e.g. "
          ^ String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") backends))
    ~docv:"BACKEND" ["target"]
  |> Arg.value

let default_cmd =
  let doc = "" in
  let man = [
    `P {|
  TODO our incredibly helpful documentation.
  |}
  ] in
  Term.(term_result (const cmd_everything $ setup_log $ target_backend $
                     unikernel_file $ unikernel_name $ output_file)),
  Term.info "noconfig" ~sdocs:Manpage.s_common_options
    ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  Term.exit (Term.eval default_cmd)
