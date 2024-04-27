open Ppxlib

let rec compile_expr loc : Syntax.Core.expr -> Parsetree.expression =
  let open Ast_builder.Default in
  function
  | Null -> [%expr lazy I.Null]
  | True -> [%expr lazy I.True]
  | False -> [%expr lazy I.False]
  | String s -> [%expr lazy (I.String [%e estring ~loc s])]
  | Number n -> [%expr lazy (I.Double [%e efloat ~loc (string_of_float n)])]
  | Array xs ->
      [%expr
        lazy (I.Array [%e xs |> List.map (compile_expr loc) |> elist ~loc])]
  | ArrayIndex (e1, e2) ->
      [%expr
        List.nth
          (I.get_array [%e compile_expr loc e1])
          (I.get_double [%e compile_expr loc e2] |> int_of_float)]
  | Binary (e1, `Add, e2) ->
      [%expr
        match ([%e compile_expr loc e1], [%e compile_expr loc e2]) with
        | (lazy (Double f1)), (lazy (Double f2)) -> lazy (Double (f1 +. f2))
        | (lazy (Array xs)), (lazy (Array ys)) -> lazy (Array (xs @ ys))
        | (lazy (String s1)), (lazy (String s2)) -> lazy (String (s1 ^ s2))
        | (lazy (Object (assrts1, fields1))), (lazy (Object (assrts2, fields2)))
          ->
            assert false (* FIXME *)
        | _ -> failwith "invalid add"]
  | Binary (e1, `Sub, e2) ->
      [%expr
        lazy
          (I.Double
             (I.get_double [%e compile_expr loc e1]
             -. I.get_double [%e compile_expr loc e2]))]
  | Binary (e1, `Mult, e2) ->
      [%expr
        lazy
          (I.Double
             (I.get_double [%e compile_expr loc e1]
             *. I.get_double [%e compile_expr loc e2]))]
  | Binary (e1, `Div, e2) ->
      [%expr
        lazy
          (I.Double
             (I.get_double [%e compile_expr loc e1]
             /. I.get_double [%e compile_expr loc e2]))]
  | Binary (e1, `And, e2) ->
      [%expr
        lazy
          (match I.get_bool [%e compile_expr loc e1] with
          | false -> I.False
          | true -> I.get_bool [%e compile_expr loc e2] |> I.value_of_bool)]
  | Binary (e1, `Or, e2) ->
      [%expr
        lazy
          (match I.get_bool [%e compile_expr loc e1] with
          | true -> I.True
          | false -> I.get_bool [%e compile_expr loc e2] |> I.value_of_bool)]
  | Binary (e1, `Land, e2) ->
      [%expr
        lazy
          (I.Double
             ((I.get_double [%e compile_expr loc e1] |> int_of_float)
              land (I.get_double [%e compile_expr loc e2] |> int_of_float)
             |> float_of_int))]
  | Binary (e1, `Lor, e2) ->
      [%expr
        lazy
          (I.Double
             ((I.get_double [%e compile_expr loc e1] |> int_of_float)
              lor (I.get_double [%e compile_expr loc e2] |> int_of_float)
             |> float_of_int))]
  | Binary (e1, `Xor, e2) ->
      [%expr
        lazy
          (I.Double
             ((I.get_double [%e compile_expr loc e1] |> int_of_float)
              lxor (I.get_double [%e compile_expr loc e2] |> int_of_float)
             |> float_of_int))]
  | Binary (e1, `Lsl, e2) ->
      [%expr
        lazy
          (I.Double
             ((I.get_double [%e compile_expr loc e1] |> int_of_float)
              lsl (I.get_double [%e compile_expr loc e2] |> int_of_float)
             |> float_of_int))]
  | Binary (e1, `Lsr, e2) ->
      [%expr
        lazy
          (I.Double
             ((I.get_double [%e compile_expr loc e1] |> int_of_float)
              lsr (I.get_double [%e compile_expr loc e2] |> int_of_float)
             |> float_of_int))]
  | Binary (e1, `Lt, e2) ->
      [%expr
        lazy
          (if I.std_cmp ([%e compile_expr loc e1], [%e compile_expr loc e2]) < 0
           then I.True
           else I.False)]
  | Binary (e1, `Le, e2) ->
      [%expr
        lazy
          (if
             I.std_cmp ([%e compile_expr loc e1], [%e compile_expr loc e2]) <= 0
           then I.True
           else I.False)]
  | Binary (e1, `Gt, e2) ->
      [%expr
        lazy
          (if I.std_cmp ([%e compile_expr loc e1], [%e compile_expr loc e2]) > 0
           then I.True
           else I.False)]
  | Binary (e1, `Ge, e2) ->
      [%expr
        lazy
          (if
             I.std_cmp ([%e compile_expr loc e1], [%e compile_expr loc e2]) >= 0
           then I.True
           else I.False)]
  | Unary (Not, e) ->
      [%expr
        lazy (I.get_bool [%e compile_expr loc e] |> not |> I.value_of_bool)]
  | Unary (Lnot, e) ->
      [%expr
        lazy
          (I.Double
             (I.get_double [%e compile_expr loc e]
             |> int_of_float |> lnot |> float_of_int))]
  | Unary (Neg, e) ->
      [%expr lazy (I.Double (-.I.get_double [%e compile_expr loc e]))]
  | Unary (Pos, e) ->
      [%expr lazy (I.Double (+.I.get_double [%e compile_expr loc e]))]
  | If (e1, e2, e3) ->
      [%expr
        lazy
          (match [%e compile_expr loc e1] with
          | (lazy True) -> Lazy.force [%e compile_expr loc e2]
          | (lazy False) -> Lazy.force [%e compile_expr loc e3]
          | _ -> failwith "invalid if condition")]
  | Function ([], body) ->
      [%expr lazy (I.Function (fun [] -> [%e compile_expr loc body]))]
  | Call (e, [], []) -> [%expr I.get_function [%e compile_expr loc e] []]
  | _ -> assert false

let compile expr =
  let loc = !Ast_helper.default_loc in
  let e = compile_expr loc expr in
  [%str
    module I = struct
      module StringMap = Map.Make (String)

      type value =
        | Null
        | True
        | False
        | String of string
        | Double of float
        | Object of (value Lazy.t list * (int * value Lazy.t list) StringMap.t)
        | Function of (value Lazy.t list -> value Lazy.t)
        | Array of value Lazy.t list

      let value_of_bool = function true -> True | false -> False

      let get_bool = function
        | (lazy True) -> true
        | (lazy False) -> false
        | _ -> failwith "expect bool got something else"

      let get_array = function
        | (lazy (Array xs)) -> xs
        | _ -> failwith "expect array got something else"

      let get_double = function
        | (lazy (Double f)) -> f
        | _ -> failwith "expect double got something else"

      let get_function = function
        | (lazy (Function f)) -> f
        | _ -> failwith "expect function got something else"

      let rec std_cmp = function
        | (lazy (Array [])), (lazy (Array [])) -> 0
        | (lazy (Array [])), (lazy (Array (_ :: _))) -> -1
        | (lazy (Array (_ :: _))), (lazy (Array [])) -> 1
        | (lazy (Array (a :: aa))), (lazy (Array (b :: bb))) ->
            let r = std_cmp (a, b) in
            if r = 0 then std_cmp (lazy (Array aa), lazy (Array bb)) else r
        | (lazy (String s1)), (lazy (String s2)) -> String.compare s1 s2
        | (lazy (Double n1)), (lazy (Double n2)) -> Float.compare n1 n2
        | _ -> failwith "std_cmp: invalid arguments"

      let manifestation =
        let open Format in
        let rec aux ppf = function
          | (lazy Null) -> fprintf ppf "null"
          | (lazy True) -> fprintf ppf "true"
          | (lazy False) -> fprintf ppf "false"
          | (lazy (String s)) -> fprintf ppf "\"%s\"" s (* FIXME: escape *)
          | (lazy (Double f)) when f = (f |> int_of_float |> float_of_int) ->
              fprintf ppf "%d" (int_of_float f)
          | (lazy (Double f)) -> fprintf ppf "%f" f
          | (lazy (Array [])) -> fprintf ppf "[ ]"
          | (lazy (Array xs)) ->
              fprintf ppf "@[<v 3>[@,%a@]@,]"
                (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@,") aux)
                xs
          | (lazy (Function _)) -> ()
          | (lazy (Object _)) -> assert false
        in
        aux Format.std_formatter
    end

    module Compiled = struct
      let e : I.value Lazy.t = [%e e]
      let () = I.manifestation e
    end]
