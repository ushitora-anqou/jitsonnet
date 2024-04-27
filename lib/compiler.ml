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
        | _ -> failwith "invalid add"]
  | Binary (e1, `Mult, e2) ->
      [%expr
        lazy
          (I.Double
             (I.get_double [%e compile_expr loc e1]
             *. I.get_double [%e compile_expr loc e2]))]
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
  | _ -> assert false

let compile Syntax.{ expr } =
  let loc = !Ast_helper.default_loc in
  let e = Syntax.desugar_expr false expr |> compile_expr loc in
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

      let rec manifestation = function
        | (lazy Null) -> "null"
        | (lazy True) -> "true"
        | (lazy False) -> "false"
        | (lazy (String s)) -> {|"|} ^ s ^ {|"|} (* FIXME: escape *)
        | (lazy (Double f)) ->
            if f |> int_of_float |> float_of_int = f then
              f |> int_of_float |> string_of_int
            else string_of_float f
        | (lazy (Array xs)) ->
            "[" ^ (xs |> List.map manifestation |> String.concat ",") ^ "]"
        | _ -> assert false
    end

    module Compiled = struct
      let e : I.value Lazy.t = [%e e]
      let () = e |> I.manifestation |> print_string
    end]
