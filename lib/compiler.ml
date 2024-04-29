open Ppxlib

type env = { vars : (string, string) Hashtbl.t; loc : location }

let gensym suffix =
  gen_symbol ~prefix:"var_" ()
  ^ "_"
  ^
  if String.starts_with ~prefix:"$" suffix then
    String.sub suffix 1 (String.length suffix - 1)
  else suffix

let with_binds env ids f =
  ids |> List.iter (fun id -> Hashtbl.add env.vars id (gensym id));
  Fun.protect
    ~finally:(fun () -> ids |> List.iter (fun id -> Hashtbl.remove env.vars id))
    f

let rec compile_expr ({ loc; _ } as env) :
    Syntax.Core.expr -> Parsetree.expression =
  let open Ast_builder.Default in
  let pexp_let ~loc recflag binds body =
    match binds with [] -> body | _ -> pexp_let ~loc recflag binds body
  in
  function
  | Null -> [%expr I.Null]
  | True -> [%expr I.True]
  | False -> [%expr I.False]
  | String s -> [%expr I.String [%e estring ~loc s]]
  | Number n -> [%expr I.Double [%e efloat ~loc (string_of_float n)]]
  | Array xs ->
      [%expr
        I.Array [%e xs |> List.map (compile_expr_lazy env) |> pexp_array ~loc]]
  | ArrayIndex (e1, e2) ->
      [%expr
        match [%e compile_expr env e1] with
        | I.Array a ->
            a.(I.get_double [%e compile_expr env e2] |> int_of_float)
            |> Lazy.force
        | I.Object (_, tbl) -> (
            let key = I.get_string [%e compile_expr env e2] in
            match Hashtbl.find_opt tbl key with
            | None -> failwith ("field does not exist: " ^ key)
            | Some (_, (lazy v)) -> v)
        | _ -> failwith "ArrayIndex: expect array got something else"]
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
        match lhs with
        | I.Double f1 -> I.Double (f1 +. I.get_double (rhs super))
        | I.Array xs -> I.Array (Array.append xs (I.get_array (rhs super)))
        | I.String s1 -> I.String (s1 ^ I.get_string (rhs super))
        | I.Object (assrts1, fields1) ->
            let super = lazy lhs in
            let assrts2, fields2 = I.get_object (rhs super) in
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
            I.Object (assrts1 @ assrts2, tbl)
        | _ -> failwith "invalid add"]
  | Binary (e1, `Sub, e2) ->
      [%expr
        I.Double
          (I.get_double [%e compile_expr env e1]
          -. I.get_double [%e compile_expr env e2])]
  | Binary (e1, `Mult, e2) ->
      [%expr
        I.Double
          (I.get_double [%e compile_expr env e1]
          *. I.get_double [%e compile_expr env e2])]
  | Binary (e1, `Div, e2) ->
      [%expr
        I.Double
          (I.get_double [%e compile_expr env e1]
          /. I.get_double [%e compile_expr env e2])]
  | Binary (e1, `And, e2) ->
      [%expr
        match I.get_bool [%e compile_expr env e1] with
        | false -> I.False
        | true -> I.get_bool [%e compile_expr env e2] |> I.value_of_bool]
  | Binary (e1, `Or, e2) ->
      [%expr
        match I.get_bool [%e compile_expr env e1] with
        | true -> I.True
        | false -> I.get_bool [%e compile_expr env e2] |> I.value_of_bool]
  | Binary (e1, `Land, e2) ->
      [%expr
        I.Double
          ((I.get_double [%e compile_expr env e1] |> int_of_float)
           land (I.get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lor, e2) ->
      [%expr
        I.Double
          ((I.get_double [%e compile_expr env e1] |> int_of_float)
           lor (I.get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Xor, e2) ->
      [%expr
        I.Double
          ((I.get_double [%e compile_expr env e1] |> int_of_float)
           lxor (I.get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lsl, e2) ->
      [%expr
        I.Double
          ((I.get_double [%e compile_expr env e1] |> int_of_float)
           lsl (I.get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lsr, e2) ->
      [%expr
        I.Double
          ((I.get_double [%e compile_expr env e1] |> int_of_float)
           lsr (I.get_double [%e compile_expr env e2] |> int_of_float)
          |> float_of_int)]
  | Binary (e1, `Lt, e2) ->
      [%expr
        if I.std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) < 0
        then I.True
        else I.False]
  | Binary (e1, `Le, e2) ->
      [%expr
        if I.std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) <= 0
        then I.True
        else I.False]
  | Binary (e1, `Gt, e2) ->
      [%expr
        if I.std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) > 0
        then I.True
        else I.False]
  | Binary (e1, `Ge, e2) ->
      [%expr
        if I.std_cmp ([%e compile_expr env e1], [%e compile_expr env e2]) >= 0
        then I.True
        else I.False]
  | Unary (Not, e) ->
      [%expr I.get_bool [%e compile_expr env e] |> not |> I.value_of_bool]
  | Unary (Lnot, e) ->
      [%expr
        I.Double
          (I.get_double [%e compile_expr env e]
          |> int_of_float |> lnot |> float_of_int)]
  | Unary (Neg, e) -> [%expr I.Double (-.I.get_double [%e compile_expr env e])]
  | Unary (Pos, e) -> [%expr I.Double (+.I.get_double [%e compile_expr env e])]
  | If (e1, e2, e3) ->
      [%expr
        match [%e compile_expr env e1] with
        | True -> [%e compile_expr env e2]
        | False -> [%e compile_expr env e3]
        | _ -> failwith "invalid if condition"]
  | Function (params, body) ->
      with_binds env (params |> List.map fst) @@ fun () ->
      let binds =
        params
        |> List.mapi @@ fun i (id, e) ->
           value_binding ~loc
             ~pat:(ppat_var ~loc { loc; txt = Hashtbl.find env.vars id })
             ~expr:
               [%expr
                 if [%e eint ~loc i] < Array.length positional then
                   positional.([%e eint ~loc i])
                 else
                   match List.assoc_opt [%e estring ~loc id] named with
                   | Some x -> x
                   | None -> lazy [%e compile_expr env e]]
      in
      [%expr
        I.Function
          (fun (positional, named) ->
            [%e pexp_let ~loc Nonrecursive binds (compile_expr_lazy env body)])]
  | Call (e, positional, named) ->
      [%expr
        I.get_function [%e compile_expr env e]
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
        I.manifestation Format.str_formatter v;
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
  | Object (assrts, fields) ->
      let fields (* compile keys with outer env *) =
        fields
        |> List.map (fun (e1, Syntax.H h, e2) -> (compile_expr env e1, h, e2))
      in

      with_binds env [ "self" ] @@ fun () ->
      [%expr
        let rec [%p pvar ~loc (Hashtbl.find env.vars "self")] =
          lazy
            (I.Object
               ( [%e assrts |> List.map (compile_expr_lazy env) |> elist ~loc],
                 let tbl = Hashtbl.create 0 in
                 [%e
                   fields
                   |> List.fold_left
                        (fun e (e1, h, e2) ->
                          [%expr
                            match [%e e1] with
                            | I.Null -> [%e e]
                            | I.String k ->
                                Hashtbl.add tbl k
                                  ( [%e eint ~loc h],
                                    lazy [%e compile_expr env e2] );
                                [%e e]
                            | _ ->
                                failwith
                                  "field name must be string, got something \
                                   else"])
                        [%expr tbl]] ))
        in
        Lazy.force [%e evar ~loc (Hashtbl.find env.vars "self")]]
  | ObjectFor (e1, e2, x, e3) ->
      let compiled_e3 (* with env *) = compile_expr env e3 in
      with_binds env [ x ] @@ fun () ->
      let compiled_e1 (* with env + x *) = compile_expr env e1 in
      with_binds env [ "self" ] @@ fun () ->
      let compiled_e2 (* with env + x, self *) = compile_expr env e2 in
      [%expr
        let rec [%p pvar ~loc (Hashtbl.find env.vars "self")] =
          lazy
            (I.Object
               ( [],
                 [%e compiled_e3] |> I.get_array |> Array.to_seq
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
                              match [%e compiled_e1] with
                              | I.Null -> None
                              | I.String s ->
                                  Some (s, (1, lazy [%e compiled_e2]))
                              | _ ->
                                  failwith
                                    "field name must be string, got something \
                                     else"]])
                 |> Hashtbl.of_seq ))
        in
        Lazy.force [%e evar ~loc (Hashtbl.find env.vars "self")]]

and compile_expr_lazy ({ loc; _ } as env) e =
  [%expr lazy [%e compile_expr env e]]

let compile expr =
  let loc = !Ast_helper.default_loc in
  let env = { loc; vars = Hashtbl.create 0 } in
  with_binds env [ "super"; "std" ] @@ fun () ->
  let e = compile_expr env expr in
  let open Ast_builder.Default in
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

      let manifestation =
        let open Format in
        let rec aux ppf = function
          | Null -> fprintf ppf "null"
          | True -> fprintf ppf "true"
          | False -> fprintf ppf "false"
          | String s -> fprintf ppf "\"%s\"" s (* FIXME: escape *)
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
          | Object (assrts, h) when Hashtbl.length h = 0 ->
              assrts |> List.iter (fun (lazy _) -> ());
              fprintf ppf "{ }"
          | Object (assrts, tbl) ->
              assrts |> List.iter (fun (lazy _) -> ());
              let xs =
                tbl |> Hashtbl.to_seq |> List.of_seq
                |> List.filter_map (fun (k, (h, v)) ->
                       if h = 2 then None else Some (k, v))
                |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
              in
              fprintf ppf "@[<v 3>{@,%a@]@,}"
                (pp_print_list
                   ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
                   (fun ppf (k, (lazy v)) ->
                     fprintf ppf "@<0>\"@<0>%s@<0>\"@<0>:@<0> %a" k aux v))
                xs
        in
        aux

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
    end

    module Compiled = struct
      let [%p pvar ~loc (Hashtbl.find env.vars "super")] =
        lazy (I.Object ([], Hashtbl.create 0))

      let [%p pvar ~loc (Hashtbl.find env.vars "std")] =
        lazy
          (I.Object
             ( [],
               let tbl = Hashtbl.create 10 in
               Hashtbl.add tbl "primitiveEquals"
                 (1, lazy (I.Function I.std_primitive_equals));
               Hashtbl.add tbl "length" (1, lazy (I.Function I.std_length));
               Hashtbl.add tbl "makeArray"
                 (1, lazy (I.Function I.std_make_array));
               Hashtbl.add tbl "type" (1, lazy (I.Function I.std_type));
               Hashtbl.add tbl "filter" (1, lazy (I.Function I.std_filter));
               Hashtbl.add tbl "objectHasEx"
                 (1, lazy (I.Function I.std_object_has_ex));
               Hashtbl.add tbl "objectFieldsEx"
                 (1, lazy (I.Function I.std_object_fields_ex));
               tbl ))

      let e : I.value = [%e e]
      let () = I.manifestation Format.std_formatter e
    end]
