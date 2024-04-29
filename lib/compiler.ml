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
        | Array a ->
            a.(I.get_double [%e compile_expr env e2] |> int_of_float)
            |> Lazy.force
        | Object (_, tbl) -> (
            let key = I.get_string [%e compile_expr env e2] in
            match Hashtbl.find_opt tbl key with
            | None -> failwith ("field does not exist: " ^ key)
            | Some (_, (lazy v)) -> v)
        | _ -> failwith "ArrayIndex: expect array got something else"]
  | Binary (e1, `Add, e2) ->
      [%expr
        match ([%e compile_expr env e1], [%e compile_expr env e2]) with
        | I.Double f1, I.Double f2 -> I.Double (f1 +. f2)
        | I.Array xs, I.Array ys -> I.Array (Array.append xs ys)
        | I.String s1, I.String s2 -> I.String (s1 ^ s2)
        | I.Object (assrts1, fields1), I.Object (assrts2, fields2) ->
            let tbl = Hashtbl.create 0 in
            fields1
            |> Hashtbl.iter (fun f (h, v) ->
                   match Hashtbl.find_opt fields2 f with
                   | Some _ -> ()
                   | None -> Hashtbl.add tbl f (h, v));
            fields2
            |> Hashtbl.iter (fun f (h, v) ->
                   match Hashtbl.find_opt fields1 f with
                   | Some _ -> ()
                   | None -> Hashtbl.add tbl f (h, v));
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
      let outer_self = Hashtbl.find_opt env.vars "self" in

      with_binds env [ "self"; "super" ] @@ fun () ->
      [%expr
        let [%p pvar ~loc (Hashtbl.find env.vars "super")] =
          [%e
            match outer_self with
            | Some outer_self -> evar ~loc outer_self
            | None -> [%expr lazy (I.Object ([], Hashtbl.create 0))]]
        in
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
      let outer_self = Hashtbl.find_opt env.vars "self" in
      with_binds env [ "self"; "super" ] @@ fun () ->
      let compiled_e2 (* with env + x, self, super *) = compile_expr env e2 in
      [%expr
        let [%p pvar ~loc (Hashtbl.find env.vars "super")] =
          [%e
            match outer_self with
            | Some outer_self -> evar ~loc outer_self
            | None -> [%expr lazy (I.Object ([], Hashtbl.create 0))]]
        in
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
  let e = compile_expr env expr in
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
          | Object (_ (* FIXME *), h) when Hashtbl.length h = 0 ->
              fprintf ppf "{ }"
          | Object (_ (* FIXME *), tbl) ->
              let xs =
                tbl |> Hashtbl.to_seq |> List.of_seq
                |> List.filter_map (fun (k, (h, v)) ->
                       if h = 2 then None else Some (k, v))
              in
              fprintf ppf "@[<v 3>{@,%a@]@,}"
                (pp_print_list
                   ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
                   (fun ppf (k, (lazy v)) ->
                     fprintf ppf "@<0>\"@<0>%s@<0>\"@<0>:@<0> %a" k aux v))
                xs
        in
        aux
    end

    module Compiled = struct
      let e : I.value = [%e e]
      let () = I.manifestation Format.std_formatter e
    end]
