open Ppxlib

let compile_expr loc : Syntax.Core.expr -> Parsetree.expression =
  let open Ast_builder.Default in
  function
  | Null -> [%expr lazy I.Null]
  | True -> [%expr lazy I.True]
  | False -> [%expr lazy I.False]
  | String s -> [%expr lazy (I.String [%e estring ~loc s])]
  | Number n -> [%expr lazy (I.Double [%e efloat ~loc (string_of_float n)])]
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

      let manifestation = function
        | (lazy Null) -> "null"
        | (lazy True) -> "true"
        | (lazy False) -> "false"
        | (lazy (String s)) -> {|"|} ^ s ^ {|"|} (* FIXME: escape *)
        | (lazy (Double f)) ->
            if f |> int_of_float |> float_of_int = f then
              f |> int_of_float |> string_of_int
            else string_of_float f
        | _ -> assert false
    end

    module Compiled = struct
      let e : I.value Lazy.t = [%e e]
      let () = e |> I.manifestation |> print_string
    end]
