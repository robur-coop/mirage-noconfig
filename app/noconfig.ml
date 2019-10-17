let filter_none (type a) : a option list -> a option list =
  fun a -> List.filter (function None -> false | Some _ -> true) a

let parse_job =
  let open Parsetree in
  let rec long_unwind acc = let open Longident in function
      | Lident plain -> String.concat "." @@ plain::acc
      | Ldot (x,y) -> long_unwind (y::acc) x
    | Lapply (x, _y) -> long_unwind ("complex"::acc) x
  in
  let rec fold_function acc pvb_expr =
    match pvb_expr.pexp_desc with
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var name; _}, exp) ->
      fold_function (name.Asttypes.txt::acc) exp
    | _ -> acc
    in
  let rec fold_functor acc = function
    | Pmod_functor (name, typ , next) ->
      (*fold_functor ((name, typ) :: acc) next.pmod_desc*)
      fold_functor ((None, name.txt, match typ with
        | None -> "NONETODO"
        | Some x -> begin match x.pmty_desc with
            | Pmty_ident {txt ; _ } ->
              long_unwind [] txt
            | _ -> "TODO too complicated"
            end
        ) :: acc) next.pmod_desc
    | Pmod_structure lst ->
      let start = List.fold_left (fun acc si ->
          match si.pstr_desc with
          | Pstr_value (_rec, vbs) ->
            List.fold_left (fun acc vb ->
                (* a value binding list *)
                begin match vb.pvb_pat.ppat_desc with
                  | Ppat_var thing ->
                    if thing.Asttypes.txt <> "start"
                    then acc
                    else
                      fold_function [] vb.pvb_expr
                  | _ -> acc
                end) acc vbs
          | _ -> acc) [] lst
                  |>  List.rev in
      begin match List.rev acc with
      | [] -> acc
      | (_ (* todo overriding previous start on Some _ *),
         name, typ)::tl -> (Some start, name, typ) :: tl
      end
    | _ -> failwith "fancy stuff not implemented"
  in
  function
  | { Parsetree.pstr_desc = Pstr_module { pmb_name = job_name ;
                                          pmb_expr ; _} ; _ } ->
    begin match pmb_expr.pmod_desc with
      | Pmod_functor _ as first ->
        Some (job_name.txt,
              fold_functor [] first)
      | _ -> None
    end
  | _ -> None

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
  D.StringSet.filter (fun x -> String.sub x 0
                         (min 7 @@ String.length x) = "Mirage_")
    !D.free_structure_names
  |> D.StringSet.elements

let () =
  let parsed =
  Ocaml_common.Pparse.parse_implementation Fmt.stdout ~tool_name:"noconfig"
    Sys.argv.(1)
  in
  topl_functors parsed ;
  let jobs = List.fold_left (fun acc expr ->
      match parse_job expr with
      | None -> acc
      | Some job -> job::acc
    ) [] parsed in
  Fmt.pr "\nExternal modules:\n%a\n----\n"
    Fmt.(list ~sep:(unit"\n")string) @@ ext_mirage_depend parsed ;

  List.iter (fun (job_name, job) ->
      Fmt.pr "\n-- Job %S in %S:\n" job_name Sys.argv.(1);
      List.iter (function
        (None, _name, _typ) -> Fmt.pr " -> %s : %s" _name _typ
          | (Some args, _name, _typ) ->
            Fmt.pr "(fun %a) -> %s:%s"
              Fmt.(list ~sep:(unit" ") string) args
              _name _typ
        ) job
      ;
      Fmt.pr "\n"
    ) jobs

(*  Fmt.pr "%a"
    Ocaml_common.Printast.implementation parsed
*)
