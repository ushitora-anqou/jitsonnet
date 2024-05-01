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
  | Error e ->
      [%expr
        let v = [%e compile_expr env e] in
        manifestation Format.str_formatter v;
        failwith (Format.flush_str_formatter ())]
  | Local (binds, e) ->
      with_binds env (binds |> List.map fst) @@ fun () ->
      pexp_let ~loc Recursive
        (binds
        |> List.map @@ fun (id, e) ->
           value_binding ~loc
             ~pat:(ppat_var ~loc { loc; txt = Hashtbl.find env.vars id })
             ~expr:(compile_expr_lazy env e))
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
                 ~expr:(compile_expr_lazy env e))
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
                       fields
                       |> List.fold_left
                            (fun e (e1, h, e2) ->
                              [%expr
                                object_field tbl [%e eint ~loc h]
                                  (lazy [%e compile_expr env e2])
                                  [%e e1];
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
      let compiled_e2 (* with env + x, self *) = compile_expr env e2 in
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
                              object_field'
                                (lazy [%e compiled_e2])
                                [%e compiled_e1]]])
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

and compile_expr_lazy ({ loc; _ } as env) e =
  [%expr lazy [%e compile_expr env e]]

let compile root_prog_path progs bins strs =
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
             ~expr:[%expr lazy [%e compile_expr env e]])
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
    module I = struct
      type value =
        | Null
        | True
        | False
        | String of string
        | Double of float
        | Object of (value Lazy.t list * (string, int * value Lazy.t) Hashtbl.t)
        | Function of
            (value Lazy.t array * (string * value Lazy.t) list -> value Lazy.t)
        | Array of value Lazy.t array

      let value_of_bool = function true -> True | false -> False

      let get_object = function
        | Object obj -> obj
        | _ -> failwith "expect object got something else"

      let get_bool = function
        | True -> true
        | False -> false
        | _ -> failwith "expect bool got something else"

      let get_array = function
        | Array xs -> xs
        | _ -> failwith "expect array got something else"

      let get_double = function
        | Double f -> f
        | _ -> failwith "expect double got something else"

      let get_function = function
        | Function f -> f
        | _ -> failwith "expect function got something else"

      let get_string = function
        | String s -> s
        | _ -> failwith "expect string got something else"

      let rec std_cmp = function
        | Array a, Array b ->
            let rec aux ia ib =
              match (ia = Array.length a, ib = Array.length b) with
              | true, true -> 0
              | true, false -> -1
              | false, true -> 1
              | false, false ->
                  let r = std_cmp (Lazy.force a.(ia), Lazy.force b.(ib)) in
                  if r = 0 then aux (ia + 1) (ib + 1) else r
            in
            aux 0 0
        | String s1, String s2 -> String.compare s1 s2
        | Double n1, Double n2 -> Float.compare n1 n2
        | _ -> failwith "std_cmp: invalid arguments"

      let manifestation ppf v =
        let open Format in
        let rec aux ppf = function
          | Null -> fprintf ppf "null"
          | True -> fprintf ppf "true"
          | False -> fprintf ppf "false"
          | String s -> fprintf ppf "%S" s
          | Double f when f = (f |> int_of_float |> float_of_int) ->
              fprintf ppf "%d" (int_of_float f)
          | Double f -> fprintf ppf "%f" f
          | Array [||] -> fprintf ppf "[ ]"
          | Array xs ->
              fprintf ppf "@[<v 3>[@,%a@]@,]"
                (pp_print_array
                   ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
                   (fun ppf (lazy x) -> aux ppf x))
                xs
          | Function _ -> ()
          | Object (assrts, tbl) ->
              assrts |> List.iter (fun (lazy _) -> ());
              let xs =
                tbl |> Hashtbl.to_seq |> List.of_seq
                |> List.filter_map (fun (k, (h, v)) ->
                       if h = 2 then None else Some (k, v))
                |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
              in
              if xs = [] then fprintf ppf "{ }"
              else
                fprintf ppf "@[<v 3>{@,%a@]@,}"
                  (pp_print_list
                     ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
                     (fun ppf (k, (lazy v)) ->
                       fprintf ppf "@<0>\"@<0>%s@<0>\"@<0>:@<0> %a" k aux v))
                  xs
        in
        aux ppf v;
        fprintf ppf "\n"

      let std_primitive_equals ([| v; v' |], []) =
        lazy
          ((match (Lazy.force v, Lazy.force v') with
           | String lhs, String rhs -> lhs = rhs
           | Double lhs, Double rhs -> lhs = rhs
           | True, True | False, False | Null, Null -> true
           | _ -> false)
          |> value_of_bool)

      let std_length ([| v |], []) =
        lazy
          (match v with
          | (lazy (Array xs)) -> Double (xs |> Array.length |> float_of_int)
          | (lazy (String s)) -> Double (s |> String.length |> float_of_int)
          | (lazy (Object (_, fields))) ->
              Double
                (fields |> Hashtbl.to_seq
                |> Seq.filter_map (fun (f, (h, _)) ->
                       if h = 2 then None else Some ())
                |> Seq.length |> float_of_int)
          | _ -> failwith "std.length: invalid type argument")

      let std_make_array ([| n; f |], []) =
        lazy
          (let n = n |> Lazy.force |> get_double in
           let f = f |> Lazy.force |> get_function in
           Array
             (Array.init (int_of_float n) (fun i ->
                  f ([| lazy (Double (float_of_int i)) |], []))))

      let std_type ([| v |], []) =
        lazy
          (match v with
          | (lazy Null) -> String "null"
          | (lazy (True | False)) -> String "boolean"
          | (lazy (String _)) -> String "string"
          | (lazy (Function _)) -> String "function"
          | (lazy (Double _)) -> String "number"
          | (lazy (Object _)) -> String "object"
          | (lazy (Array _)) -> String "array")

      let std_filter ([| f; ary |], []) =
        lazy
          (let f = f |> Lazy.force |> get_function in
           let xs = ary |> Lazy.force |> get_array in
           Array
             (xs |> Array.to_list
             |> List.filter (fun x -> f ([| x |], []) |> Lazy.force |> get_bool)
             |> Array.of_list))

      let std_object_has_ex ([| obj; f; b' |], []) =
        lazy
          (let _, fields = obj |> Lazy.force |> get_object in
           let f = f |> Lazy.force |> get_string in
           let b' = b' |> Lazy.force |> get_bool in
           match Hashtbl.find_opt fields f with
           | Some (h, _) when h <> 2 || b' -> True
           | _ -> False)

      let std_object_fields_ex ([| obj; b' |], []) =
        lazy
          (let b' = b' |> Lazy.force |> get_bool in
           let _, fields = obj |> Lazy.force |> get_object in
           Array
             (fields |> Hashtbl.to_seq
             |> Seq.filter_map (fun (f, (h, _)) ->
                    if h <> 2 || b' then Some f else None)
             |> List.of_seq |> List.sort String.compare |> Array.of_list
             |> Array.map (fun x -> lazy (String x))))

      let array_index f1 f2 =
        match f1 () with
        | Array a -> a.(get_double (f2 ()) |> int_of_float) |> Lazy.force
        | Object (_, tbl) -> (
            let key = get_string (f2 ()) in
            match Hashtbl.find_opt tbl key with
            | None -> failwith ("field does not exist: " ^ key)
            | Some (_, (lazy v)) -> v)
        | _ -> failwith "ArrayIndex: expect array got something else"

      let binary_add super lhs rhs =
        match lhs with
        | Double f1 -> Double (f1 +. get_double (rhs super))
        | Array xs -> Array (Array.append xs (get_array (rhs super)))
        | String s1 -> String (s1 ^ get_string (rhs super))
        | Object (assrts1, fields1) ->
            let super = lazy lhs in
            let assrts2, fields2 = get_object (rhs super) in
            let tbl = Hashtbl.create 0 in
            let common = ref [] in
            fields1
            |> Hashtbl.iter (fun f (h, v) ->
                   match Hashtbl.find_opt fields2 f with
                   | Some (h', v') -> common := (f, h, v, h', v') :: !common
                   | None -> Hashtbl.add tbl f (h, v));
            fields2
            |> Hashtbl.iter (fun f (h, v) ->
                   match Hashtbl.find_opt fields1 f with
                   | Some _ -> ()
                   | None -> Hashtbl.add tbl f (h, v));
            !common
            |> List.iter (fun (f, h1, v1, h2, v2) ->
                   let h = if h2 = 1 then h1 else h2 in
                   Hashtbl.add tbl f (h, v2));
            Object (assrts1 @ assrts2, tbl)
        | _ -> failwith "invalid add"

      let object_field tbl h v = function
        | Null -> ()
        | String k -> Hashtbl.add tbl k (h, v)
        | _ -> failwith "field name must be string, got something else"

      let object_field' v = function
        | Null -> None
        | String s -> Some (s, (1, v))
        | _ -> failwith "field name must be string, got something else"

      let function_param i positional id named v =
        if i < Array.length positional then positional.(i)
        else
          match List.assoc_opt id named with
          | Some x -> x
          | None -> (
              match v with
              | Some v -> v
              | None -> failwith "Parameter not bound")

      let if_ f1 f2 f3 =
        match f1 () with
        | True -> f2 ()
        | False -> f3 ()
        | _ -> failwith "invalid if condition"
    end

    open I

    module Compiled = struct
      let [%p pvar ~loc (Hashtbl.find env.vars "super")] =
        lazy (Object ([], Hashtbl.create 0))

      let [%p pvar ~loc (Hashtbl.find env.vars "std")] =
        lazy
          (Object
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
               tbl ))

      let () =
        [%e
          pexp_let ~loc Recursive
            (progs_bindings @ bins_bindings @ strs_bindings)
            [%expr
              manifestation Format.std_formatter
                (Lazy.force
                   [%e
                     evar ~loc
                       (Hashtbl.find env.vars
                          (get_import_id `Import root_prog_path))])]]
    end]
