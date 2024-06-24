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
  | exception _ -> Error "read_all: can't open the file"
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> Ok (In_channel.input_all ic))

let write_all file_path body =
  match open_out_bin file_path with
  | exception _ -> Error "write_all: can't open the file"
  | oc ->
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> Ok (Out_channel.output_string oc body))

let execute_process ?(interactive = false) ?env prog args =
  Logs.debug (fun m ->
      m "execute_process: %s %s"
        (env |> Option.fold ~none:[] ~some:Array.to_list |> String.concat " ")
        (Filename.quote_command prog (Array.to_list args)));
  let env = match env with None -> Unix.environment () | Some env -> env in
  let stdin_in_fd, stdin_out_fd = Unix.pipe ~cloexec:true () in
  let stdout_in_fd, stdout_out_fd = Unix.pipe ~cloexec:true () in
  let stderr_in_fd, stderr_out_fd = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () ->
      Unix.close stdout_in_fd;
      Unix.close stderr_in_fd)
  @@ fun () ->
  let pid =
    Unix.create_process_env prog args env
      (if interactive then Unix.stdin else stdin_in_fd)
      (if interactive then Unix.stdout else stdout_out_fd)
      (if interactive then Unix.stderr else stderr_out_fd)
  in
  Unix.close stdin_out_fd;
  Unix.close stdout_out_fd;
  Unix.close stderr_out_fd;
  let stdout_content =
    if interactive then ""
    else In_channel.input_all (Unix.in_channel_of_descr stdout_in_fd)
  in
  let stderr_content =
    if interactive then ""
    else In_channel.input_all (Unix.in_channel_of_descr stderr_in_fd)
  in
  let _, status = Unix.waitpid [] pid in
  (status, stdout_content, stderr_content)

exception Compilation_failed of string
exception Execution_failed of string

let string_of_process_status = function
  | Unix.WEXITED n -> "exited " ^ string_of_int n
  | WSIGNALED n -> "signal " ^ string_of_int n
  | WSTOPPED n -> "stopped  " ^ string_of_int n

let compile_to_native ~ghc ~runtime_dir ~main_hs ~main_exe ~interactive () =
  let run_ghc args =
    match execute_process ~interactive ghc (Array.of_list (ghc :: args)) with
    | Unix.WEXITED 0, _, _ -> ()
    | status, stdout_content, stderr_content ->
        raise
          (Compilation_failed
             (Printf.sprintf "%s failed: %s:\n%s\n%s" ghc
                (string_of_process_status status)
                stdout_content stderr_content))
  in
  try
    run_ghc [ "-i" ^ runtime_dir; "-c"; "-ffull-laziness"; main_hs ];
    run_ghc
      (List.flatten
         [
           [ "-o"; main_exe ];
           [ "-i" ^ runtime_dir ];
           [
             Filename.concat runtime_dir "Common.o";
             Filename.concat runtime_dir "Stdjsonnet.o";
             Filename.remove_extension main_hs ^ ".o";
           ];
         ])
  with exc ->
    raise
      (Compilation_failed
         (Printf.sprintf "unexpected error: %s" (Printexc.to_string exc)))

let run_executable ~main_exe ~interactive () =
  try execute_process ~interactive main_exe [| main_exe |]
  with exc ->
    raise
      (Execution_failed
         (Printf.sprintf "unexpected error: %s" (Printexc.to_string exc)))

let timeit show_profile name f =
  match show_profile with
  | false -> f ()
  | true ->
      let t0 = Unix.gettimeofday () in
      let r = f () in
      let t1 = Unix.gettimeofday () in
      Logs.info (fun m -> m "timeit: %s: %f sec" name (t1 -. t0));
      r

type config = {
  work_dir_prefix : string; [@default "/tmp"]
  remove_work_dir : bool; [@default true]
  main_hs_file_name : string; [@default "Main.hs"]
  main_exe_file_name : string; [@default "main.exe"]
  ghc : string; [@default "ghc"]
  show_profile : bool; [@default false]
  interactive_compile : bool;
  interactive_execute : bool;
  runtime_dir : string;
}
[@@deriving make]

let execute
    {
      work_dir_prefix;
      remove_work_dir;
      main_hs_file_name;
      main_exe_file_name;
      ghc;
      show_profile;
      interactive_execute;
      interactive_compile;
      runtime_dir;
      _;
    } body =
  let work_dir = Filename.temp_dir ~temp_dir:work_dir_prefix "jitsonnet_" "" in
  Fun.protect ~finally:(fun () -> if remove_work_dir then rm_rf work_dir)
  @@ fun () ->
  let main_hs = Filename.concat work_dir main_hs_file_name in
  let main_exe = Filename.concat work_dir main_exe_file_name in

  let oc = open_out_bin main_hs in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      Out_channel.output_string oc body;
      Out_channel.flush oc);

  timeit show_profile "compile to native (haskell)" (fun () ->
      compile_to_native ~ghc ~main_hs ~main_exe ~interactive:interactive_compile
        ~runtime_dir ());
  timeit show_profile "execute native (haskell)" (fun () ->
      run_executable ~main_exe ~interactive:interactive_execute ())
