open Ppxlib

type env = { vars : (string, string) Hashtbl.t; loc : location }

let gensym suffix = gen_symbol ~prefix:"var_" () ^ "_" ^ suffix

let with_binds env ids f =
  ids |> List.iter (fun id -> Hashtbl.add env.vars id (gensym id));
  Fun.protect
    ~finally:(fun () -> ids |> List.iter (fun id -> Hashtbl.remove env.vars id))
    f

let rec compile_expr ({ loc; _ } as env) :
    Syntax.Core.expr -> Parsetree.expression =
  let open Ast_builder.Default in
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
        | _ -> failwith "ArrayIndex: expect array got something else"]
  | Binary (e1, `Add, e2) ->
      [%expr
        match ([%e compile_expr env e1], [%e compile_expr env e2]) with
        | I.Double f1, I.Double f2 -> I.Double (f1 +. f2)
        | I.Array xs, I.Array ys -> I.Array (Array.append xs ys)
        | I.String s1, I.String s2 -> I.String (s1 ^ s2)
        | I.Object (assrts1, fields1), I.Object (assrts2, fields2) ->
            assert false (* FIXME *)
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
  | Function (params, body) -> (
      with_binds env (params |> List.map fst) @@ fun () ->
      match
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
      with
      | [] -> [%expr I.Function (fun (_, _) -> [%e compile_expr_lazy env body])]
      | binds ->
          [%expr
            I.Function
              (fun (positional, named) ->
                [%e pexp_let ~loc Recursive binds (compile_expr_lazy env body)])]
      )
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
  | Error _ -> [%expr failwith "fixme: error"]
  | Local (binds, e) ->
      with_binds env (binds |> List.map fst) @@ fun () ->
      pexp_let ~loc Recursive
        (binds
        |> List.map @@ fun (id, e) ->
           value_binding ~loc
             ~pat:(ppat_var ~loc { loc; txt = Hashtbl.find env.vars id })
             ~expr:(compile_expr_lazy env e))
        (compile_expr env e)
  | Var id ->
      [%expr
        Lazy.force
          [%e
            evar ~loc
              (match Hashtbl.find_opt env.vars id with
              | Some s -> s
              | None -> failwith ("missing variable: " ^ id))]]
  | Object _ -> [%expr I.Object ([], Hashtbl.create 0)]
  | ObjectFor _ -> assert false
  | Self -> assert false
  | Super -> assert false

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
        | Object of
            (value Lazy.t list * (string, int * value Lazy.t list) Hashtbl.t)
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
          | Object (_, h) when Hashtbl.length h = 0 -> fprintf ppf "{ }"
          | Object _ -> assert false
        in
        aux Format.std_formatter
    end

    module Compiled = struct
      let e : I.value = [%e e]
      let () = I.manifestation e
    end]
