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

let main_ml_file_name = "main.ml"
let main_exe_file_name = "main.exe"
let ocamlopt_stdout_file_name = "ocamlopt.stdout"
let ocamlopt_stderr_file_name = "ocamlopt.stderr"
let main_exe_stdout_file_name = "main.exe.stdout"
let main_exe_stderr_file_name = "main.exe.stderr"

let execute' ~dir_name ~ocamlopt_path ~redirect ast =
  let main_ml = Filename.concat dir_name main_ml_file_name in
  let main_exe = Filename.concat dir_name main_exe_file_name in
  let redirect_ocamlopt =
    if redirect then
      Printf.sprintf "> %s 2> %s"
        (Filename.quote (Filename.concat dir_name ocamlopt_stdout_file_name))
        (Filename.quote (Filename.concat dir_name ocamlopt_stderr_file_name))
    else ""
  in
  let redirect_main_exe =
    if redirect then
      Printf.sprintf "> %s 2> %s"
        (Filename.quote (Filename.concat dir_name main_exe_stdout_file_name))
        (Filename.quote (Filename.concat dir_name main_exe_stderr_file_name))
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
     let com =
       Filename.quote_command ocamlopt_path [ "-o"; main_exe; main_ml ]
     in
     let com = com ^ redirect_ocamlopt in
     Unix.system com
   with
  | WEXITED 0 -> ()
  | _ -> failwith "ocamlopt failed");
  (match
     let com = Filename.quote_command main_exe [] in
     let com = com ^ redirect_main_exe in
     Unix.system com
   with
  | WEXITED 0 -> ()
  | _ -> failwith "compiled executable failed");
  ()

let execute ?(remove_tmp_dir = true) ast =
  let temp_dir =
    if remove_tmp_dir then None else Some "/tmp" (* for dune runtest *)
  in
  let dir_name = Filename.temp_dir ?temp_dir "jitsonnet" "" in
  Fun.protect ~finally:(fun () -> if remove_tmp_dir then rm_rf dir_name)
  @@ fun () ->
  execute' ~dir_name ~ocamlopt_path:"ocamlopt" ~redirect:true ast;
  let ic = open_in_bin (Filename.concat dir_name main_exe_stdout_file_name) in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  In_channel.input_all ic

let execute_from_cli ast =
  let dir_name = Filename.temp_dir "jitsonnet" "" in
  Fun.protect ~finally:(fun () -> rm_rf dir_name) @@ fun () ->
  execute' ~dir_name ~ocamlopt_path:"ocamlopt" ~redirect:false ast;
  ()
