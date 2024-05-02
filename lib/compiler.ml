open Ppxlib
open Ast_builder.Default

type env = { vars : (string, string) Hashtbl.t; loc : location }

let gensym suffix =
  gen_symbol ~prefix:"var_" ()
  ^ "_"
  ^
  let suffix =
    if String.length suffix < 10 then suffix else String.sub suffix 0 10
  in
  let suffix = suffix |> Str.(global_replace (regexp {|[^a-zA-Z0-9]|}) "_") in
  suffix

let with_binds env ids f =
  ids |> List.iter (fun id -> Hashtbl.add env.vars id (gensym id));
  Fun.protect
    ~finally:(fun () -> ids |> List.iter (fun id -> Hashtbl.remove env.vars id))
    f

let get_import_id kind file_path =
  (match kind with
  | `Import -> "prog/"
  | `Importbin -> "bin/"
  | `Importstr -> "str/")
  ^ file_path

let pexp_let ~loc recflag binds body =
  match binds with [] -> body | _ -> pexp_let ~loc recflag binds body

let rec compile_expr ({ loc; _ } as env) :
    Syntax.Core.expr -> Parsetree.expression = function
  | Null -> [%expr Null]
  | True -> [%expr True]
  | False -> [%expr False]
  | String s -> [%expr String [%e estring ~loc s]]
  | Number n -> [%expr Double [%e efloat ~loc (string_of_float n)]]
  | Array xs ->
      [%expr
        Array [%e xs |> List.map (compile_expr_lazy env) |> pexp_array ~loc]]
  | ArrayIndex (e1, e2) ->
      [%expr
        array_index
          (fun () -> [%e compile_expr env e1])
          (fun () -> [%e compile_expr env e2])]
  | Binary (e1, `Add, e2) ->
      let new_super_id = gensym "super" in
      [%expr
        let super = [%e evar ~loc (Hashtbl.find env.vars "super")] in
        let lhs = [%e compile_expr env e1] in
        let rhs [%p pvar ~loc new_super_id] =
          [%e
            Hashtbl.add env.vars "super" new_super_id;
            Fun.protect ~finally:(fun () -> Hashtbl.remove env.vars "super")
            @@ fun () -> compile_expr env e2]
        in
        binary_add super lhs rhs]
  | Binary (e1, `Sub, e2) ->
      [%expr
        Double
          (get_double [%e compile_expr env e1]
          -. get_double [%e compile_expr env e2])]
  | Binary (e1, `Mult, e2) ->
      [%expr
        Double
          (get_double [%e compile_expr env e1]
          *. get_double [%e compile_expr env e2])]
  | Binary (e1, `Div, e2) ->
      [%expr
        Double
          (get_double [%e compile_expr env e1]
          /. get_double [%e compile_expr env e2])]
  | Binary (e1, `And, e2) ->
      [%expr
        match get_bool [%e compile_expr env e1] with
        | false -> False
        | true -> get_bool [%e compile_expr env e2] |> value_of_bool]
  | Binary (e1, `Or, e2) ->
      [%expr
        match get_bool [%e compile_expr env e1] with
        | true -> True
        | false -> get_bool [%e compile_expr env e2] |> value_of_bool]
  | Binary (e1, `Land, e2) ->
      [%expr
        Double
          ((get_double [%e compile_expr env e1] |> int_of_float)
           land (get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lor, e2) ->
      [%expr
        Double
          ((get_double [%e compile_expr env e1] |> int_of_float)
           lor (get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Xor, e2) ->
      [%expr
        Double
          ((get_double [%e compile_expr env e1] |> int_of_float)
           lxor (get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lsl, e2) ->
      [%expr
        Double
          ((get_double [%e compile_expr env e1] |> int_of_float)
           lsl (get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lsr, e2) ->
      [%expr
        Double
          ((get_double [%e compile_expr env e1] |> int_of_float)
           lsr (get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lt, e2) ->
      [%expr
        if std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) < 0 then
          True
        else False]
  | Binary (e1, `Le, e2) ->
      [%expr
        if std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) <= 0
        then True
        else False]
  | Binary (e1, `Gt, e2) ->
      [%expr
        if std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) > 0 then
          True
        else False]
  | Binary (e1, `Ge, e2) ->
      [%expr
        if std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) >= 0
        then True
        else False]
  | Unary (Not, e) ->
      [%expr get_bool [%e compile_expr env e] |> not |> value_of_bool]
  | Unary (Lnot, e) ->
      [%expr
        Double
          (get_double [%e compile_expr env e]
          |> int_of_float |> lnot |> float_of_int)]
  | Unary (Neg, e) -> [%expr Double (-.get_double [%e compile_expr env e])]
  | Unary (Pos, e) -> [%expr Double (+.get_double [%e compile_expr env e])]
  | If (e1, e2, e3) ->
      [%expr
        if_
          (fun () -> [%e compile_expr env e1])
          (fun () -> [%e compile_expr env e2])
          (fun () -> [%e compile_expr env e3])]
  | Function (params, body) ->
      with_binds env (params |> List.map fst) @@ fun () ->
      let binds =
        params
        |> List.mapi @@ fun i (id, e) ->
           value_binding ~loc
             ~pat:(ppat_var ~loc { loc; txt = Hashtbl.find env.vars id })
             ~expr:
               [%expr
                 function_param [%e eint ~loc i] positional [%e estring ~loc id]
                   named
                   [%e
                     match e with
                     | None -> [%expr None]
                     | Some e -> [%expr Some (lazy [%e compile_expr env e])]]]
      in
      [%expr
        Function
          (fun (positional, named) ->
            [%e pexp_let ~loc Nonrecursive binds (compile_expr_lazy env body)])]
  | Call (e, positional, named) ->
      [%expr
        get_function [%e compile_expr env e]
          ( [%e pexp_array ~loc (positional |> List.map (compile_expr_lazy env))],
            [%e
              named
              |> List.map (fun (id, e) ->
                     pexp_tuple ~loc
                       [ estring ~loc id; compile_expr_lazy env e ])
              |> elist ~loc] )
        |> Lazy.force]
  | Error e -> [%expr error [%e compile_expr env e]]
  | Local (binds, e) ->
      with_binds env (binds |> List.map fst) @@ fun () ->
      pexp_let ~loc Recursive
        (binds
        |> List.map @@ fun (id, e) ->
           value_binding ~loc
             ~pat:(ppat_var ~loc { loc; txt = Hashtbl.find env.vars id })
             ~expr:(compile_expr_lazy ~in_bind:true env e))
        (compile_expr env e)
  | Self -> compile_expr env (Var "self")
  | Super -> compile_expr env (Var "super")
  | Var id ->
      [%expr
        Lazy.force
          [%e
            evar ~loc
              (match Hashtbl.find_opt env.vars id with
              | Some s -> s
              | None -> failwith ("missing variable: " ^ id))]]
  | Object { binds; assrts; fields } ->
      let fields (* compile keys with outer env *) =
        fields
        |> List.map (fun (e1, Syntax.H h, e2) -> (compile_expr env e1, h, e2))
      in

      with_binds env ("self" :: (binds |> List.map fst)) @@ fun () ->
      let bindings =
        binds
        |> List.map (fun (id, e) ->
               value_binding ~loc
                 ~pat:(ppat_var ~loc { loc; txt = Hashtbl.find env.vars id })
                 ~expr:(compile_expr_lazy ~in_bind:true env e))
      in
      let bindings =
        value_binding ~loc
          ~pat:(pvar ~loc (Hashtbl.find env.vars "self"))
          ~expr:
            [%expr
              lazy
                (Object
                   ( [%e
                       assrts |> List.map (compile_expr_lazy env) |> elist ~loc],
                     let tbl = Hashtbl.create 0 in
                     [%e
                       fields |> List.rev
                       |> List.fold_left
                            (fun e (e1, h, e2) ->
                              [%expr
                                object_field tbl [%e eint ~loc h] [%e e1]
                                  [%e compile_expr_lazy env e2];
                                [%e e]])
                            [%expr tbl]] ))]
        :: bindings
      in
      pexp_let ~loc Recursive bindings
        [%expr Lazy.force [%e evar ~loc (Hashtbl.find env.vars "self")]]
  | ObjectFor (e1, e2, x, e3) ->
      let compiled_e3 (* with env *) = compile_expr env e3 in
      with_binds env [ x ] @@ fun () ->
      let compiled_e1 (* with env + x *) = compile_expr env e1 in
      with_binds env [ "self" ] @@ fun () ->
      let compiled_e2 (* with env + x, self *) = compile_expr_lazy env e2 in
      [%expr
        let rec [%p pvar ~loc (Hashtbl.find env.vars "self")] =
          lazy
            (Object
               ( [],
                 [%e compiled_e3] |> get_array |> Array.to_seq
                 |> Seq.filter_map (fun v ->
                        [%e
                          pexp_let ~loc Nonrecursive
                            [
                              value_binding ~loc
                                ~pat:
                                  (ppat_var ~loc
                                     { loc; txt = Hashtbl.find env.vars x })
                                ~expr:[%expr v];
                            ]
                            [%expr
                              object_field' [%e compiled_e1] [%e compiled_e2]]])
                 |> Hashtbl.of_seq ))
        in
        Lazy.force [%e evar ~loc (Hashtbl.find env.vars "self")]]
  | (Import file_path | Importbin file_path | Importstr file_path) as node ->
      let import_id =
        get_import_id
          (match node with
          | Import _ -> `Import
          | Importbin _ -> `Importbin
          | Importstr _ -> `Importstr
          | _ -> assert false)
          file_path
      in
      [%expr Lazy.force [%e evar ~loc (Hashtbl.find env.vars import_id)]]

and compile_expr_lazy ?(in_bind = false) ({ loc; _ } as env) e =
  match compile_expr env e with
  | {
   pexp_desc =
     Parsetree.Pexp_apply
       ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Lazy", "force"); _ }; _ },
         [ (Nolabel, ({ pexp_desc = Pexp_ident _; _ } as var)) ] );
   _;
  }
    when not in_bind ->
      var
  | e -> [%expr lazy [%e e]]

let compile ?(target = `Main) root_prog_path progs bins strs =
  let loc = !Ast_helper.default_loc in
  let env = { loc; vars = Hashtbl.create 0 } in

  let bind_ids =
    "super" :: "std"
    :: ((progs |> List.map (fun (path, _) -> get_import_id `Import path))
       @ (bins |> List.map (get_import_id `Importbin))
       @ (strs |> List.map (get_import_id `Importstr)))
  in
  with_binds env bind_ids @@ fun () ->
  let progs_bindings =
    progs
    |> List.map (fun (path, e) ->
           value_binding ~loc
             ~pat:
               (ppat_var ~loc
                  {
                    loc;
                    txt = Hashtbl.find env.vars (get_import_id `Import path);
                  })
             ~expr:[%expr [%e compile_expr_lazy ~in_bind:true env e]])
  in
  let bins_bindings =
    bins
    |> List.map (fun path ->
           value_binding ~loc
             ~pat:
               (ppat_var ~loc
                  {
                    loc;
                    txt = Hashtbl.find env.vars (get_import_id `Importbin path);
                  })
             ~expr:
               [%expr
                 lazy
                   (Array
                      (let path = [%e estring ~loc path] in
                       let ic =
                         try open_in_bin path
                         with _ -> failwith ("cannot open file: " ^ path)
                       in
                       Fun.protect
                         ~finally:(fun () -> close_in ic)
                         (fun () ->
                           let rec loop acc =
                             match In_channel.input_byte ic with
                             | None -> acc
                             | Some b ->
                                 loop (lazy (Double (float_of_int b)) :: acc)
                           in
                           loop [] |> List.rev |> Array.of_list)))])
  in
  let strs_bindings =
    (* FIXME: UTF-8 check *)
    strs
    |> List.map (fun path ->
           value_binding ~loc
             ~pat:
               (ppat_var ~loc
                  {
                    loc;
                    txt = Hashtbl.find env.vars (get_import_id `Importstr path);
                  })
             ~expr:
               [%expr
                 lazy
                   (String
                      (let path = [%e estring ~loc path] in
                       let ic =
                         try open_in_bin path
                         with _ -> failwith ("cannot open file: " ^ path)
                       in
                       Fun.protect
                         ~finally:(fun () -> close_in ic)
                         (fun () -> In_channel.input_all ic)))])
  in

  [%str
    open Common

    module Compiled = struct
      let [%p pvar ~loc (Hashtbl.find env.vars "super")] =
        lazy (Object ([], Hashtbl.create 0))

      let [%p pvar ~loc (Hashtbl.find env.vars "std")] =
        lazy
          (let primitive_std =
             Object
               ( [],
                 let tbl = Hashtbl.create 10 in
                 Hashtbl.add tbl "primitiveEquals"
                   (1, lazy (Function std_primitive_equals));
                 Hashtbl.add tbl "length" (1, lazy (Function std_length));
                 Hashtbl.add tbl "makeArray" (1, lazy (Function std_make_array));
                 Hashtbl.add tbl "type" (1, lazy (Function std_type));
                 Hashtbl.add tbl "filter" (1, lazy (Function std_filter));
                 Hashtbl.add tbl "objectHasEx"
                   (1, lazy (Function std_object_has_ex));
                 Hashtbl.add tbl "objectFieldsEx"
                   (1, lazy (Function std_object_fields_ex));
                 tbl )
           in
           [%e
             match target with
             | `Stdjsonnet -> [%expr primitive_std]
             | _ ->
                 [%expr
                   binary_add
                     (lazy (assert false))
                     primitive_std
                     (fun _ -> Lazy.force Stdjsonnet.Compiled.v)]])

      let v =
        [%e
          pexp_let ~loc Recursive
            (progs_bindings @ bins_bindings @ strs_bindings)
            (evar ~loc
               (Hashtbl.find env.vars (get_import_id `Import root_prog_path)))]

      let () =
        [%e
          match target with
          | `Main -> [%expr manifestation Format.std_formatter (Lazy.force v)]
          | _ -> [%expr ()]]
    end]
