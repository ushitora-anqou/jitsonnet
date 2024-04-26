open Ppxlib

let compile_expr loc : Syntax.Core.expr -> Parsetree.expression = function
  | True -> [%expr true]
  | _ -> assert false

let compile Syntax.{ expr } =
  let loc = !Ast_helper.default_loc in
  let e = Syntax.desugar_expr false expr |> compile_expr loc in
  [%str
    module Compiled = struct
      let _ = [%e e]
    end]
