open Ppxlib
open Ast_builder.Default

type var_desc =
  | VarUnknown
  | VarObject of { key_to_value_ary_index : (string, int) Hashtbl.t }

type var_in_env = { name_in_target : string; desc : var_desc }

type env = {
  vars : (string, var_in_env) Hashtbl.t;
  loc : location;
  is_stdjsonnet : bool;
}

let env_evar ~loc env name =
  evar ~loc (Hashtbl.find env.vars name).name_in_target

let env_pvar ~loc env name =
  pvar ~loc (Hashtbl.find env.vars name).name_in_target

let set_same_var_desc env name ~same_as:name' =
  let var = Hashtbl.find env.vars name in
  let var' = Hashtbl.find env.vars name' in
  let var = { var with desc = var'.desc } in
  Hashtbl.replace env.vars name var

let set_var_object env name ~key_to_value_ary_index () =
  let var = Hashtbl.find env.vars name in
  let var = { var with desc = VarObject { key_to_value_ary_index } } in
  Hashtbl.replace env.vars name var

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
  ids
  |> List.iter (fun id ->
         Hashtbl.add env.vars id
           { name_in_target = gensym id; desc = VarUnknown });
  Fun.protect
    ~finally:(fun () -> ids |> List.iter (fun id -> Hashtbl.remove env.vars id))
    f

let get_import_id kind file_path =
  (match kind with
  | `Import -> "prog/"
  | `Importbin -> "bin/"
  | `Importstr -> "str/")
  ^ file_path

let get_ext_code_id key = "extCode/" ^ key

let pexp_let ~loc recflag binds body =
  match binds with [] -> body | _ -> pexp_let ~loc recflag binds body

let compile_builtin_std loc = function
  | "length" -> Ok [%expr std_length]
  | "makeArray" -> Ok [%expr std_make_array]
  | "type" -> Ok [%expr std_type]
  | "primitiveEquals" -> Ok [%expr std_primitive_equals]
  | "filter" -> Ok [%expr std_filter]
  | "objectHasEx" -> Ok [%expr std_object_has_ex]
  | "objectFieldsEx" -> Ok [%expr std_object_fields_ex]
  | "modulo" -> Ok [%expr std_modulo]
  | "codepoint" -> Ok [%expr std_codepoint]
  | "char" -> Ok [%expr std_char]
  | "floor" -> Ok [%expr std_floor]
  | "acos" -> Ok [%expr std_acos]
  | "asin" -> Ok [%expr std_asin]
  | "atan" -> Ok [%expr std_atan]
  | "pow" -> Ok [%expr std_pow]
  | "ceil" -> Ok [%expr std_ceil]
  | "cos" -> Ok [%expr std_cos]
  | "sin" -> Ok [%expr std_sin]
  | "tan" -> Ok [%expr std_tan]
  | "exp" -> Ok [%expr std_exp]
  | "log" -> Ok [%expr std_log]
  | "sqrt" -> Ok [%expr std_sqrt]
  | "exponent" -> Ok [%expr std_exponent]
  | "mantissa" -> Ok [%expr std_mantissa]
  | "md5" -> Ok [%expr std_md5]
  | _ -> Error "not found"

let rec compile_expr ?toplevel:_ ({ loc; is_stdjsonnet; _ } as env) :
    Syntax.Core.expr -> Parsetree.expression = function
  | Null -> [%expr Null]
  | True -> [%expr True]
  | False -> [%expr False]
  | String s -> [%expr SmartString (SmartString.of_string [%e estring ~loc s])]
  | Number n -> [%expr Double [%e efloat ~loc (string_of_float n)]]
  | Array xs ->
      [%expr
        Array [%e xs |> List.map (compile_expr_lazy env) |> pexp_array ~loc]]
  | ArrayIndex es ->
      let rec aux times es =
        match es with
        | Syntax.Core.Var v, Syntax.Core.String key when times < 1 -> (
            match (Hashtbl.find env.vars v).desc with
            | VarUnknown -> aux 1 es
            | VarObject { key_to_value_ary_index } -> (
                match Hashtbl.find_opt key_to_value_ary_index key with
                | None -> aux 1 es
                | Some value_ary_index ->
                    [%expr
                      let (Object (General ((value_ary, _, _), _))) =
                        Lazy.force [%e env_evar ~loc env v]
                      in
                      Lazy.force value_ary.([%e eint ~loc value_ary_index])]))
        | e1, String s ->
            [%expr
              array_index_s [%e compile_expr_lazy env e1] [%e estring ~loc s]]
        | e1, e2 ->
            [%expr
              array_index [%e compile_expr env e1] [%e compile_expr env e2]]
      in
      aux 0 es
  | Binary (e1, `Add, e2) ->
      [%expr
        let lhs = [%e compile_expr env e1] in
        let rhs = [%e compile_expr env e2] in
        binary_add lhs rhs]
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
          (Int64.to_float
             (Int64.logand
                (Int64.of_float (get_double [%e compile_expr env e1]))
                (Int64.of_float (get_double [%e compile_expr env e2]))))]
  | Binary (e1, `Lor, e2) ->
      [%expr
        Double
          (Int64.to_float
             (Int64.logor
                (Int64.of_float (get_double [%e compile_expr env e1]))
                (Int64.of_float (get_double [%e compile_expr env e2]))))]
  | Binary (e1, `Xor, e2) ->
      [%expr
        Double
          (Int64.to_float
             (Int64.logxor
                (Int64.of_float (get_double [%e compile_expr env e1]))
                (Int64.of_float (get_double [%e compile_expr env e2]))))]
  | Binary (e1, `Lsl, e2) ->
      [%expr
        Double
          (Int64.to_float
             (Int64.shift_left
                (Int64.of_float (get_double [%e compile_expr env e1]))
                (int_of_float (get_double [%e compile_expr env e2]))))]
  | Binary (e1, `Lsr, e2) ->
      [%expr
        Double
          (Int64.to_float
             (Int64.shift_right
                (Int64.of_float (get_double [%e compile_expr env e1]))
                (int_of_float (get_double [%e compile_expr env e2]))))]
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
        if_ [%e compile_expr env e1]
          (fun () -> [%e compile_expr env e2])
          (fun () -> [%e compile_expr env e3])]
  | Function (params, body) ->
      let use_rec_value =
        params |> List.exists (fun (_, v) -> Option.is_some v)
        (* FIXME: use more strict condition *)
      in
      with_binds env (params |> List.map fst) @@ fun () ->
      let binds =
        params
        |> List.mapi @@ fun i (id, e) ->
           value_binding ~loc ~pat:(env_pvar ~loc env id)
             ~expr:
               (let body =
                  [%expr
                    function_param [%e eint ~loc i] positional
                      [%e estring ~loc id] named
                      [%e
                        match e with
                        | None -> [%expr None]
                        | Some e -> [%expr Some (lazy [%e compile_expr env e])]]]
                in
                if use_rec_value then [%expr lazy (Lazy.force [%e body])]
                else body)
      in
      [%expr
        Function
          (fun (positional, named) ->
            [%e
              pexp_let ~loc
                (if use_rec_value then Recursive else Nonrecursive)
                binds (compile_expr env body)])]
  | Call
      ( ArrayIndex (Var std, String "equals"),
        ([ e; String s ] | [ String s; e ]),
        [] )
    when std = "$std" || (std = "std" && is_stdjsonnet) ->
      [%expr std_equals_string [%e compile_expr_lazy env e] [%e estring ~loc s]]
  | Call ((ArrayIndex (Var std, String name) as e), positional, named)
    when std = "$std" || (std = "std" && is_stdjsonnet) -> (
      match (name, positional, named) with
      | "equals", ([ e; Null ] | [ Null; e ]), [] ->
          [%expr std_equals_null [%e compile_expr_lazy env e]]
      | "equals", ([ e; True ] | [ True; e ]), [] ->
          [%expr std_equals_boolean [%e compile_expr_lazy env e] true]
      | "equals", ([ e; False ] | [ False; e ]), [] ->
          [%expr std_equals_boolean [%e compile_expr_lazy env e] false]
      | "equals", ([ e; String s ] | [ String s; e ]), [] ->
          [%expr
            std_equals_string [%e compile_expr_lazy env e] [%e estring ~loc s]]
      | ( "equals",
          ( [ e; Number n ]
          | [ Number n; e ]
          | [ e; Unary (Pos, Number n) ]
          | [ Unary (Pos, Number n); e ] ),
          [] ) ->
          [%expr
            std_equals_number [%e compile_expr_lazy env e]
              [%e efloat ~loc (string_of_float n)]]
      | ( "equals",
          ([ e; Unary (Neg, Number n) ] | [ Unary (Neg, Number n); e ]),
          [] ) ->
          [%expr
            std_equals_number [%e compile_expr_lazy env e]
              (-.[%e efloat ~loc (string_of_float n)])]
      | _ -> (
          match compile_builtin_std loc name with
          | Ok func ->
              [%expr [%e func] [%e compile_call_args env positional named]]
          | Error _ -> compile_generic_call env (e, positional, named)))
  | Call call -> compile_generic_call env call
  | Error e -> [%expr error [%e compile_expr env e]]
  | Local (binds, e) ->
      with_binds env (binds |> List.map fst) @@ fun () ->
      let bindings =
        binds
        |> List.map @@ fun (id, e) ->
           (match e with
           | Syntax.Core.Var id' -> set_same_var_desc env id ~same_as:id'
           | Syntax.Core.Object { fields; _ } ->
               set_var_object env id
                 ~key_to_value_ary_index:
                   (fields
                   |> List.mapi (fun i v -> (i, v))
                   |> List.filter_map (fun (i, (e1, _, _, _)) ->
                          match e1 with
                          | Syntax.Core.String key -> Some (key, i)
                          | _ -> None)
                   |> List.to_seq |> Hashtbl.of_seq)
                 ()
           | _ -> ());
           value_binding ~loc ~pat:(env_pvar ~loc env id)
             ~expr:(compile_expr_lazy ~in_bind:true env e)
      in
      let body = compile_expr env e in
      pexp_let ~loc Recursive bindings body
  | Self -> [%expr make_simple_object ([||], [], [%e env_evar ~loc env "self"])]
  | Var id -> [%expr Lazy.force [%e env_evar ~loc env id]]
  | Object { binds; assrts; fields } ->
      let fields (* compile keys with outer env *) =
        fields
        |> List.map (fun (e1, b, Syntax.H h, e2) ->
               ( Syntax.gensym () (* variable name for e1 *),
                 compile_expr env e1,
                 b,
                 h,
                 e2 ))
      in

      with_binds env
        ("self" :: "super"
        :: ((fields |> List.map (fun (k, _, _, _, _) -> k))
           @ (binds |> List.map fst)))
      @@ fun () ->
      let body =
        pexp_let ~loc Nonrecursive
          (fields
          |> List.map (fun (id, e1, _, _, _) ->
                 value_binding ~loc ~pat:(env_pvar ~loc env id) ~expr:e1))
          (pexp_let ~loc Recursive
             (binds
             |> List.map (fun (id, e) ->
                    value_binding ~loc ~pat:(env_pvar ~loc env id)
                      ~expr:(compile_expr_lazy ~in_bind:true env e)))
             [%expr
               let value_ary =
                 [%e
                   pexp_array ~loc
                     (fields
                     |> List.map (fun (e1_key, _, plus, _, e2) ->
                            if plus then
                              [%expr
                                object_field_plus_value
                                  [%e env_evar ~loc env "super"]
                                  [%e env_evar ~loc env e1_key]
                                  [%e compile_expr env e2]]
                            else compile_expr_lazy env e2))]
               in
               ( value_ary,
                 [%e assrts |> List.map (compile_expr_lazy env) |> elist ~loc],
                 let tbl = [%e env_evar ~loc env "self"] in
                 [%e
                   fields
                   |> List.mapi (fun i (e1_key, _, _, h, _) -> (i, e1_key, h))
                   |> List.rev
                   |> List.fold_left
                        (fun e (i, e1_key, h) ->
                          [%expr
                            object_field tbl [%e eint ~loc h]
                              [%e env_evar ~loc env e1_key]
                              value_ary.([%e eint ~loc i]);
                            [%e e]])
                        [%expr tbl]] )])
      in
      [%expr
        make_object
          (fun [%p env_pvar ~loc env "self"] [%p env_pvar ~loc env "super"] ->
            [%e body])]
  | ObjectFor (e1, e2, x, e3) ->
      let compiled_e3 (* with env *) = compile_expr env e3 in
      with_binds env [ x ] @@ fun () ->
      let compiled_e1 (* with env + x *) = compile_expr env e1 in
      with_binds env [ "self"; "super" ] @@ fun () ->
      let compiled_e2 (* with env + x, self *) = compile_expr_lazy env e2 in
      [%expr
        make_object
          (fun [%p env_pvar ~loc env "self"] [%p env_pvar ~loc env "super"] ->
            let tbl = [%e env_evar ~loc env "self"] in
            [%e compiled_e3] |> get_array |> Array.to_seq
            |> Seq.filter_map (fun v ->
                   [%e
                     pexp_let ~loc Nonrecursive
                       [
                         value_binding ~loc ~pat:(env_pvar ~loc env x)
                           ~expr:[%expr v];
                       ]
                       [%expr object_field' [%e compiled_e1] [%e compiled_e2]]])
            |> Hashtbl.add_seq tbl;
            ([||], [], tbl))]
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
      [%expr Lazy.force [%e env_evar ~loc env import_id]]
  | InSuper e ->
      [%expr in_super [%e env_evar ~loc env "super"] [%e compile_expr env e]]
  | SuperIndex e ->
      [%expr super_index [%e env_evar ~loc env "super"] [%e compile_expr env e]]

and compile_expr_lazy ?(toplevel = false) ?(in_bind = false) ({ loc; _ } as env)
    e =
  match e with
  | Null | True | False | String _ | Number _ | Array _ | Function _ | Self
  | Object _ | ObjectFor _
    when not in_bind ->
      [%expr Lazy.from_val [%e compile_expr env e]]
  | _ -> (
      match compile_expr ~toplevel env e with
      | {
       pexp_desc =
         Parsetree.Pexp_apply
           ( {
               pexp_desc = Pexp_ident { txt = Ldot (Lident "Lazy", "force"); _ };
               _;
             },
             [ (Nolabel, ({ pexp_desc = Pexp_ident _; _ } as var)) ] );
       _;
      }
        when not in_bind ->
          var
      | e -> [%expr lazy [%e e]])

and compile_generic_call ({ loc; _ } as env) (e, positional, named) =
  [%expr
    get_function [%e compile_expr env e]
      [%e compile_call_args env positional named]]

and compile_call_args ({ loc; _ } as env) positional named =
  [%expr
    [%e pexp_array ~loc (positional |> List.map (compile_expr_lazy env))],
      [%e
        named
        |> List.map (fun (id, e) ->
               pexp_tuple ~loc [ estring ~loc id; compile_expr_lazy env e ])
        |> elist ~loc]]

let compile ?multi ?(string = false) ?(target = `Main) root_prog_path progs bins
    strs ext_codes =
  let loc = !Ast_helper.default_loc in
  let env =
    { loc; vars = Hashtbl.create 0; is_stdjsonnet = target = `Stdjsonnet }
  in

  let bind_ids =
    "$std"
    :: ((progs |> List.map (fun (path, _) -> get_import_id `Import path))
       @ (bins |> List.map (get_import_id `Importbin))
       @ (strs |> List.map (get_import_id `Importstr))
       @ (ext_codes |> List.map fst |> List.map get_ext_code_id))
  in
  with_binds env bind_ids @@ fun () ->
  let progs_bindings =
    progs
    |> List.map (fun (path, e) ->
           value_binding ~loc
             ~pat:(env_pvar ~loc env (get_import_id `Import path))
             ~expr:
               [%expr [%e compile_expr_lazy ~toplevel:true ~in_bind:true env e]])
  in
  let bins_bindings =
    bins
    |> List.map (fun path ->
           value_binding ~loc
             ~pat:(env_pvar ~loc env (get_import_id `Importbin path))
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
             ~pat:(env_pvar ~loc env (get_import_id `Importstr path))
             ~expr:
               [%expr
                 lazy
                   (SmartString
                      (SmartString.of_string
                         (let path = [%e estring ~loc path] in
                          let ic =
                            try open_in_bin path
                            with _ -> failwith ("cannot open file: " ^ path)
                          in
                          Fun.protect
                            ~finally:(fun () -> close_in ic)
                            (fun () -> In_channel.input_all ic))))])
  in
  let ext_codes_bindings =
    ext_codes
    |> List.map (function
         | key, Error msg ->
             value_binding ~loc
               ~pat:(env_pvar ~loc env (get_ext_code_id key))
               ~expr:[%expr lazy (failwith [%e estring ~loc msg])]
         | key, Ok code ->
             value_binding ~loc
               ~pat:(env_pvar ~loc env (get_ext_code_id key))
               ~expr:(compile_expr_lazy ~toplevel:true ~in_bind:true env code))
  in
  let other_bindings =
    [
      value_binding ~loc ~pat:(env_pvar ~loc env "$std")
        ~expr:
          [%expr
            lazy
              [%e
                match target with
                | `Stdjsonnet ->
                    [%expr make_simple_object ([||], [], empty_obj_fields)]
                | _ ->
                    [%expr
                      make_object (fun self super ->
                          let f =
                            get_object_f (Lazy.force Stdjsonnet.Compiled.v)
                          in
                          let _, assrts, _ = f self super in
                          append_to_std self;
                          Hashtbl.add self "extVar" (1, std_ext_var);
                          ([||], assrts, self))]]];
      value_binding ~loc
        ~pat:[%pat? std_ext_var]
        ~expr:
          [%expr
            lazy
              (Function
                 (make_std_ext_var
                    (Hashtbl.of_seq
                       (List.to_seq
                          [%e
                            ext_codes
                            |> List.map (fun (key, _) ->
                                   pexp_tuple ~loc
                                     [
                                       estring ~loc key;
                                       env_evar ~loc env (get_ext_code_id key);
                                     ])
                            |> elist ~loc]))))];
    ]
  in

  [%str
    open Common

    module Compiled = struct
      let v =
        [%e
          pexp_let ~loc Recursive
            (other_bindings @ progs_bindings @ bins_bindings @ strs_bindings
           @ ext_codes_bindings)
            (env_evar ~loc env (get_import_id `Import root_prog_path))]

      let () =
        [%e
          match target with
          | `Main when Option.is_some multi ->
              let target_dir =
                if Filename.is_relative (Option.get multi) then
                  Filename.concat (Sys.getcwd ()) (Option.get multi)
                else Option.get multi
              in
              [%expr
                multi_manifestation ~target_dir:[%e estring ~loc target_dir]
                  ~string:[%e ebool ~loc string] v]
          | `Main when string -> [%expr string_manifestation (Lazy.force v)]
          | `Main -> [%expr manifestation Format.std_formatter (Lazy.force v)]
          | _ -> [%expr ()]]
    end]
