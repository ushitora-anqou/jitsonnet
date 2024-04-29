open Ppxlib

(* `rm_rf dir_name` removes all files in `dir_name` and remove `dir_name`.
 * NOTE: `rm_rf` does NOT remove directories in `dir_name`.
 *)
let rm_rf dir_name =
  let h = Unix.opendir dir_name in
  let rec loop () =
    match Unix.readdir h with
    | entry ->
        (try Unix.unlink (Filename.concat dir_name entry) with _ -> ());
        loop ()
    | exception End_of_file -> ()
  in
  loop ();
  Unix.closedir h;
  (try Unix.rmdir dir_name with _ -> ());
  ()

let read_all file_path =
  match open_in_bin file_path with
  | exception _ -> Error "can't open the file"
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> Ok (In_channel.input_all ic))

let main_ml_file_name = "main.ml"
let main_exe_file_name = "main.exe"
let ocamlc_stdout_file_name = "ocamlc.stdout"
let ocamlc_stderr_file_name = "ocamlc.stderr"
let main_exe_stdout_file_name = "main.exe.stdout"
let main_exe_stderr_file_name = "main.exe.stderr"

exception Compiled_executable_failed of (Unix.process_status * string)
exception Compilation_failed of (Unix.process_status * string)

let execute' ~dir_name ~ocamlc_path ~redirect ast =
  let main_ml = Filename.concat dir_name main_ml_file_name in
  let main_exe = Filename.concat dir_name main_exe_file_name in
  let main_exe_stdout = Filename.concat dir_name main_exe_stdout_file_name in
  let main_exe_stderr = Filename.concat dir_name main_exe_stderr_file_name in
  let ocamlc_stdout = Filename.concat dir_name ocamlc_stdout_file_name in
  let ocamlc_stderr = Filename.concat dir_name ocamlc_stderr_file_name in
  let redirect_ocamlc =
    if redirect then
      Printf.sprintf "> %s 2> %s"
        (Filename.quote ocamlc_stdout)
        (Filename.quote ocamlc_stderr)
    else ""
  in
  let redirect_main_exe =
    if redirect then
      Printf.sprintf "> %s 2> %s"
        (Filename.quote main_exe_stdout)
        (Filename.quote main_exe_stderr)
    else ""
  in
  let oc = open_out_bin main_ml in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      let f = Format.formatter_of_out_channel oc in
      Pprintast.structure f ast;
      Format.pp_print_flush f ());
  (match
     let com = Filename.quote_command ocamlc_path [ "-o"; main_exe; main_ml ] in
     let com = com ^ redirect_ocamlc in
     Unix.system com
   with
  | WEXITED 0 -> ()
  | status ->
      let stderr_msg = read_all ocamlc_stderr |> Result.value ~default:"" in
      raise (Compilation_failed (status, stderr_msg)));
  (match
     let com = Filename.quote_command main_exe [] in
     let com = com ^ redirect_main_exe in
     Unix.system com
   with
  | WEXITED 0 -> ()
  | status ->
      let stderr_msg = read_all main_exe_stderr |> Result.value ~default:"" in
      raise (Compiled_executable_failed (status, stderr_msg)));
  ()

let execute ?(remove_tmp_dir = true) ast =
  let temp_dir =
    if remove_tmp_dir then None else Some "/tmp" (* for dune runtest *)
  in
  let dir_name = Filename.temp_dir ?temp_dir "jitsonnet" "" in
  Fun.protect ~finally:(fun () -> if remove_tmp_dir then rm_rf dir_name)
  @@ fun () ->
  execute' ~dir_name ~ocamlc_path:"ocamlc" ~redirect:true ast;
  read_all (Filename.concat dir_name main_exe_stdout_file_name) |> Result.get_ok

let execute_from_cli ast =
  let dir_name = Filename.temp_dir "jitsonnet" "" in
  Fun.protect ~finally:(fun () -> rm_rf dir_name) @@ fun () ->
  execute' ~dir_name ~ocamlc_path:"ocamlc" ~redirect:false ast;
  ()
