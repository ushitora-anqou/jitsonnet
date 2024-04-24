type unaryop = Lnot | Neg | Not | Pos [@@deriving show]

type binop =
  | Add
  | And
  | Div
  | Equal
  | Ge
  | Gt
  | In
  | Land
  | Le
  | Lor
  | Lsl
  | Lsr
  | Lt
  | Mod
  | Mult
  | NotEqual
  | Or
  | Sub
  | Xor
[@@deriving show]

type expr =
  | Array of expr list
  | ArrayFor of (expr * forspec * compspec)
  | ArrayIndex of (expr * expr)
  | ArraySlice of (expr * expr option * expr option * expr option)
  | Assert of (assert_ * expr)
  | Binary of (expr * binop * expr)
  | Call of (expr * args * bool (* tailstrict *))
  | Dollar
  | Error of expr
  | False
  | Function of (params * expr)
  | If of (expr * expr * expr option)
  | Import of string
  | Importbin of string
  | Importstr of string
  | InSuper of expr
  | Local of (bind list * expr)
  | Null
  | Number of float
  | Object of objinside
  | ObjectSeq of (expr * objinside)
  | Select of (expr * id)
  | Self
  | String of string
  | SuperIndex of expr
  | SuperSelect of id
  | True
  | Unary of (unaryop * expr)
  | Var of id

and id = string

and objinside =
  | ObjectFor of
      (objlocal list * expr * expr * objlocal list * forspec * compspec)
  | ObjectMemberList of member list

and member =
  | MemberAssert of assert_
  | MemberField of field
  | MemberObjlocal of objlocal

and field =
  | Field of (fieldname * bool (* +? *) * h * expr)
  | FieldFunc of (fieldname * params * h * expr)

and h = H of int
and objlocal = bind
and compspec = compspec' list
and compspec' = Forspec of forspec | Ifspec of ifspec
and forspec = id * expr
and ifspec = expr

and fieldname =
  | FieldnameExpr of expr
  | FieldnameID of id
  | FieldnameString of string

and assert_ = expr * expr option
and bind = Bind of (id * expr) | BindFunc of (id * params * expr)
and args = expr list * (id * expr) list
and params = param list
and param = id * expr option [@@deriving show]

type program = { expr : expr } [@@deriving show]

exception General_parse_error of string

module Core = struct
  type expr =
    | Array of expr list
    | Binary of (expr * binop * expr)
    | Call of (expr * expr list * (id * expr) list)
    | Error of expr
    | False
    | Function of ((id * expr) list * expr)
    | If of (expr * expr * expr)
    | Local of ((id * expr) list * expr)
    | Null
    | Number of float
    | Object of (expr list * (expr * h * expr) list)
    | ObjectFor of (expr * expr * id * expr)
    | Select of (expr * expr)
    | Self
    | String of string
    | Super
    | True
    | Unary of (unaryop * expr)
    | Var of id

  and id = string
end

let gensym =
  let i = ref 0 in
  fun () ->
    i := !i + 1;
    "$v" ^ string_of_int !i

let rec desugar_expr b = function
  | Object (ObjectMemberList members) ->
      let binds =
        members
        |> List.filter_map (function MemberObjlocal l -> Some l | _ -> None)
      in
      let binds = if b then binds else Bind ("$", Self) :: binds in
      let assrts =
        members
        |> List.filter_map (function
             | MemberAssert assrt -> Some (desugar_assert binds assrt)
             | _ -> None)
      in
      let fields =
        members
        |> List.filter_map (function
             | MemberField field -> Some (desugar_field binds b field)
             | _ -> None)
      in
      let obj = Core.Object (assrts, fields) in
      if b then
        Core.Local
          ([ ("$outerself", Core.Self); ("$outersuper", Core.Super) ], obj)
      else obj
  | Object (ObjectFor (binds, ef, ebody, binds', forspec, compspec)) ->
      let arr = gensym () in
      let xs = [] in
      let binds_xs =
        xs
        |> List.mapi (fun i x ->
               Bind (x, ArrayIndex (Var arr, Number (float_of_int i))))
      in
      Core.ObjectFor
        ( desugar_expr b (Local (binds_xs, ef)),
          desugar_expr true (Local (binds_xs @ binds @ binds', ebody)),
          arr,
          desugar_expr b (ArrayFor (Array xs, forspec, compspec)) )
  | ArrayFor (e, forspec, compspec) ->
      desugar_arrcomp e b (Forspec forspec :: compspec)
  | Local (binds, e) ->
      let binds = binds |> List.map (desugar_bind b) in
      let e = desugar_expr b e in
      Core.Local (binds, e)
  | _ -> assert false

and desugar_arrcomp _e _b = function
  (*
  | Ifspec e' :: compspec ->
      desugar_expr b (If (e', desugar_arrcomp e b compspec, Some (Array [])))
      *)
  | _ -> assert false

and desugar_assert binds = function
  | e, None -> desugar_assert binds (e, Some (String "Assertion failed"))
  | e, Some e' ->
      desugar_expr true (Local (binds, If (e, Null, Some (Error e'))))

and desugar_field binds b = function
  | Field ((FieldnameID id | FieldnameString id), plus, h, e) ->
      desugar_field binds b (Field (FieldnameExpr (String id), plus, h, e))
  | FieldFunc ((FieldnameID id | FieldnameString id), params, h, e) ->
      desugar_field binds b
        (FieldFunc (FieldnameExpr (String id), params, h, e))
  | Field (FieldnameExpr e, false, h, e') ->
      (desugar_expr b e, h, desugar_expr true (Local (binds, e')))
  | FieldFunc (FieldnameExpr e, params, h, e') ->
      desugar_field binds b
        (Field (FieldnameExpr e, false, h, Function (params, e')))
  | Field (FieldnameExpr e, true, h, e') ->
      (* FIXME
         let e'' =
           substitute [ (Var "$outerself", Self); (Var "$outersuper", Super) ] e
         in *)
      let e'' = e in
      let e''' = If (InSuper e'', Binary (SuperIndex e'', Add, e'), Some e') in
      desugar_field binds b (Field (FieldnameExpr e, false, h, e'''))

and desugar_bind b = function
  | Bind (id, e) -> (id, desugar_expr b e)
  | BindFunc (id, params, e) -> (id, desugar_expr b (Function (params, e)))
