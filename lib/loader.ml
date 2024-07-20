type t = {
  loaded : (string, string * Syntax.Core.expr) Hashtbl.t;
  importbins : (string, unit) Hashtbl.t;
  importstrs : (string, unit) Hashtbl.t;
  root_prog_path : string;
  ext_codes : (string, (Syntax.Core.expr, string) result) Hashtbl.t;
  tla_codes : (string, (Syntax.Core.expr, string) result) Hashtbl.t;
}

let list_imported_files e =
  let progs, bins, strs =
    Syntax.Core.fold
      (fun ((progs, bins, strs) as acc) e ->
        match e.v with
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

let update_imported_files root desugared =
  if root = "." then desugared
  else
    Syntax.Core.map
      (fun n ->
        match n.v with
        | Import file -> { n with v = Import (Filename.concat root file) }
        | Importbin file -> { n with v = Importbin (Filename.concat root file) }
        | Importstr file -> { n with v = Importstr (Filename.concat root file) }
        | _ -> n)
      desugared

let rec load_code ~is_stdjsonnet ~optimize code_src t =
  let ( let* ) = Result.bind in
  let* prog =
    match code_src with
    | `File (path, _real_path) -> (
        try Parser.parse_file path with Failure msg -> Error msg)
    | `Ext (key, code) ->
        Parser.parse_string ~filename:(Printf.sprintf "<extvar:%s>" key) code
    | `Tla (key, code) ->
        Parser.parse_string ~filename:(Printf.sprintf "<tlavar:%s>" key) code
  in

  let desugared = Syntax.desugar ~is_stdjsonnet prog in
  let desugared =
    match optimize with
    | false -> Syntax.Core.replace_std ~is_stdjsonnet desugared
    | true ->
        desugared
        |> Syntax.Core.alpha_conv ~is_stdjsonnet
        |> Syntax.Core.float_let_binds
  in
  let desugared =
    match code_src with
    | `File (path, _) ->
        desugared |> update_imported_files (Filename.dirname path)
    | _ -> desugared
  in

  let* () = Static_check.f is_stdjsonnet desugared in

  (match code_src with
  | `File (path, real_path) -> Hashtbl.add t.loaded real_path (path, desugared)
  | _ -> ());

  let progs, bins, strs = list_imported_files desugared in
  bins |> List.iter (fun k -> Hashtbl.replace t.importbins k ());
  strs |> List.iter (fun k -> Hashtbl.replace t.importstrs k ());

  let* () =
    progs
    |> List.fold_left
         (fun res file ->
           let* () = res in
           t |> load ~is_stdjsonnet ~optimize (`File file))
         (Ok ())
  in

  (match code_src with
  | `Ext (key, _) -> Hashtbl.replace t.ext_codes key (Ok desugared)
  | `Tla (key, _) -> Hashtbl.replace t.tla_codes key (Ok desugared)
  | _ -> ());

  Ok ()

and load_str kind key value t =
  Hashtbl.replace
    (match kind with `Ext -> t.ext_codes | `Tla -> t.tla_codes)
    key
    (Ok
       {
         v = Syntax.Core.String value;
         loc =
           {
             fname = (match kind with `Ext -> "<extstr>" | `Tla -> "<tlastr>");
             ran = None;
           };
       });
  Ok ()

and load ~is_stdjsonnet ~optimize src t =
  match src with
  | `File path -> (
      match get_real_path path with
      | Error _ ->
          Hashtbl.add t.loaded path
            ( path,
              Syntax.Core.
                {
                  loc = { fname = path; ran = None };
                  v =
                    Error
                      {
                        loc = { fname = path; ran = None };
                        v = String "can't import";
                      };
                } );
          Ok ()
      | Ok real_path -> (
          match Hashtbl.find_opt t.loaded real_path with
          | Some _ -> Ok ()
          | None ->
              load_code ~is_stdjsonnet ~optimize (`File (path, real_path)) t))
  | `Ext_code (key, prog_src) ->
      load_code ~is_stdjsonnet ~optimize (`Ext (key, prog_src)) t
  | `Tla_code (key, prog_src) ->
      load_code ~is_stdjsonnet ~optimize (`Tla (key, prog_src)) t
  | `Ext_str (key, value) -> load_str `Ext key value t
  | `Tla_str (key, value) -> load_str `Tla key value t

let compile ?multi ?string ?target t =
  Compiler.compile ?multi ?string ?target t.root_prog_path
    (t.loaded |> Hashtbl.to_seq |> List.of_seq
    |> List.map (fun (real_path, (path, e)) -> (real_path, path, e)))
    (t.importbins |> Hashtbl.to_seq_keys |> List.of_seq)
    (t.importstrs |> Hashtbl.to_seq_keys |> List.of_seq)
    (t.ext_codes |> Hashtbl.to_seq |> List.of_seq)

let compile_haskell ?multi ?string ?target t =
  Compiler_hs.compile ?multi ?string ?target t.root_prog_path
    (t.loaded |> Hashtbl.to_seq |> List.of_seq
    |> List.map (fun (real_path, (_, e)) -> (real_path, e)))
    (t.importbins |> Hashtbl.to_seq_keys |> List.of_seq)
    (t.importstrs |> Hashtbl.to_seq_keys |> List.of_seq)
    (t.ext_codes |> Hashtbl.to_seq |> List.of_seq)
    (t.tla_codes |> Hashtbl.to_seq |> List.of_seq)

let parse_code_from_cli_args ext_code =
  match String.index_opt ext_code '=' with
  | None -> (
      match Sys.getenv_opt ext_code with
      | None -> failwith ("environment variable " ^ ext_code ^ " was undefined.")
      | Some value -> (ext_code, value))
  | Some i ->
      ( String.sub ext_code 0 i,
        String.sub ext_code (i + 1) (String.length ext_code - (i + 1)) )

let load_ext_codes ~optimize ext_codes t =
  ext_codes
  |> List.iter (fun ext_code ->
         ignore
           (load ~is_stdjsonnet:false ~optimize
              (`Ext_code (parse_code_from_cli_args ext_code))
              t))

let load_ext_strs ~optimize ext_strs t =
  ext_strs
  |> List.iter (fun ext_str ->
         ignore
           (load ~is_stdjsonnet:false ~optimize
              (`Ext_str (parse_code_from_cli_args ext_str))
              t))

let load_tla_codes ~optimize tla_codes t =
  tla_codes
  |> List.iter (fun tla_code ->
         ignore
           (load ~is_stdjsonnet:false ~optimize
              (`Tla_code (parse_code_from_cli_args tla_code))
              t))

let load_tla_strs ~optimize tla_strs t =
  tla_strs
  |> List.iter (fun tla_str ->
         ignore
           (load ~is_stdjsonnet:false ~optimize
              (`Tla_str (parse_code_from_cli_args tla_str))
              t))

let load_root ?(is_stdjsonnet = false) ?(ext_codes = []) ?(ext_strs = [])
    ?(tla_codes = []) ?(tla_strs = []) ~optimize root_prog_path =
  let t =
    {
      loaded = Hashtbl.create 1;
      importbins = Hashtbl.create 0;
      importstrs = Hashtbl.create 0;
      root_prog_path;
      ext_codes = Hashtbl.create 0;
      tla_codes = Hashtbl.create 0;
    }
  in
  load_ext_codes ~optimize ext_codes t;
  load_ext_strs ~optimize ext_strs t;
  load_tla_codes ~optimize tla_codes t;
  load_tla_strs ~optimize tla_strs t;
  load ~is_stdjsonnet ~optimize (`File root_prog_path) t
  |> Result.map (Fun.const t)
