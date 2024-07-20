type t = {
  loaded : (string, string * Syntax.Core.expr) Hashtbl.t;
  importbins : (string, unit) Hashtbl.t;
  importstrs : (string, unit) Hashtbl.t;
  root_prog_path : string;
  ext_codes : (string, (Syntax.Core.expr, string) result) Hashtbl.t;
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

let rec load ~is_stdjsonnet ~optimize src t =
  let ( let* ) = Result.bind in
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
              let* prog =
                try Parser.parse_file path with Failure msg -> Error msg
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
                desugared |> update_imported_files (Filename.dirname path)
              in
              let* () = Static_check.f is_stdjsonnet desugared in
              Hashtbl.add t.loaded real_path (path, desugared);
              let progs, bins, strs = list_imported_files desugared in
              bins |> List.iter (fun k -> Hashtbl.replace t.importbins k ());
              strs |> List.iter (fun k -> Hashtbl.replace t.importstrs k ());
              progs
              |> List.fold_left
                   (fun res file ->
                     let* () = res in
                     t |> load ~is_stdjsonnet ~optimize (`File file))
                   (Ok ())))
  | `Ext_code (key, prog_src) ->
      let* prog =
        Parser.parse_string
          ~filename:(Printf.sprintf "<extvar:%s>" key)
          prog_src
      in
      let desugared =
        Syntax.desugar ~is_stdjsonnet prog
        |> Syntax.Core.alpha_conv ~is_stdjsonnet
        |> Syntax.Core.float_let_binds
      in
      let* () = Static_check.f is_stdjsonnet desugared in
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
      Hashtbl.replace t.ext_codes key (Ok desugared);
      Ok ()
  | `Ext_str (key, value) ->
      Hashtbl.replace t.ext_codes key
        (Ok
           {
             v = Syntax.Core.String value;
             loc = { fname = "<extstr>"; ran = None };
           });
      Ok ()

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

let load_ext ext_code =
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
              (`Ext_code (load_ext ext_code))
              t))

let load_ext_strs ~optimize ext_strs t =
  ext_strs
  |> List.iter (fun ext_str ->
         ignore
           (load ~is_stdjsonnet:false ~optimize (`Ext_str (load_ext ext_str)) t))

let load_root ?(is_stdjsonnet = false) ?(ext_codes = []) ?(ext_strs = [])
    ~optimize root_prog_path =
  let t =
    {
      loaded = Hashtbl.create 1;
      importbins = Hashtbl.create 0;
      importstrs = Hashtbl.create 0;
      root_prog_path;
      ext_codes = Hashtbl.create 0;
    }
  in
  load_ext_codes ~optimize ext_codes t;
  load_ext_strs ~optimize ext_strs t;
  load ~is_stdjsonnet ~optimize (`File root_prog_path) t
  |> Result.map (Fun.const t)
