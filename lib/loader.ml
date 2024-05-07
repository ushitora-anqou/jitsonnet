type t = {
  loaded : (string, Syntax.Core.expr) Hashtbl.t;
  importbins : (string, unit) Hashtbl.t;
  importstrs : (string, unit) Hashtbl.t;
  root_prog_path : string;
}

let list_imported_files e =
  let progs, bins, strs =
    Syntax.Core.fold
      (fun ((progs, bins, strs) as acc) -> function
        | Syntax.Core.Import file -> (file :: progs, bins, strs)
        | Importbin file -> (progs, file :: bins, strs)
        | Importstr file -> (progs, bins, file :: strs)
        | _ -> acc)
      ([], [], []) e
  in
  ( List.sort_uniq String.compare progs,
    List.sort_uniq String.compare bins,
    List.sort_uniq String.compare strs )

let get_real_path file_path =
  try Ok (Unix.realpath file_path)
  with _ ->
    Error
      ("get_real_path: failed to call realpath(3); maybe no entry?: "
     ^ file_path)

exception Make_imported_files_real_error of string

let make_imported_files_real root desugared =
  let conv file =
    let cand =
      if Filename.is_relative file then Filename.concat root file else file
    in
    match get_real_path cand with
    | Ok s -> s
    | Error _ -> raise (Make_imported_files_real_error file)
  in
  try
    Ok
      (Syntax.Core.map
         (function
           | Import file -> Import (conv file)
           | Importbin file -> Importbin (conv file)
           | Importstr file -> Importstr (conv file)
           | x -> x)
         desugared)
  with Make_imported_files_real_error file -> Error ("invalid import: " ^ file)

let rec load is_stdjsonnet file_path t =
  let ( let* ) = Result.bind in
  let* file_path = get_real_path file_path in
  match Hashtbl.find_opt t.loaded file_path with
  | Some _ -> Ok ()
  | None ->
      let* prog = Parser.parse_file file_path in
      let* desugared =
        Syntax.desugar prog
        |> make_imported_files_real (Filename.dirname file_path)
      in
      let* () = Static_check.f is_stdjsonnet desugared in
      Hashtbl.add t.loaded file_path desugared;
      let progs, bins, strs = list_imported_files desugared in
      bins |> List.iter (fun k -> Hashtbl.replace t.importbins k ());
      strs |> List.iter (fun k -> Hashtbl.replace t.importstrs k ());
      progs
      |> List.fold_left
           (fun res file ->
             let* () = res in
             t |> load is_stdjsonnet file)
           (Ok ())

let compile ?target t =
  Compiler.compile ?target t.root_prog_path
    (t.loaded |> Hashtbl.to_seq |> List.of_seq)
    (t.importbins |> Hashtbl.to_seq_keys |> List.of_seq)
    (t.importstrs |> Hashtbl.to_seq_keys |> List.of_seq)

let load_root is_stdjsonnet root_prog_path =
  let ( let* ) = Result.bind in
  let* root_prog_path = get_real_path root_prog_path in
  let t =
    {
      loaded = Hashtbl.create 1;
      importbins = Hashtbl.create 0;
      importstrs = Hashtbl.create 0;
      root_prog_path;
    }
  in
  load is_stdjsonnet root_prog_path t |> Result.map (Fun.const t)
