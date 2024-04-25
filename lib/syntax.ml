type unaryop = Lnot | Neg | Not | Pos [@@deriving show]

type binop =
  [ `Add
  | `And
  | `Div
  | `Equal
  | `Ge
  | `Gt
  | `In
  | `Land
  | `Le
  | `Lor
  | `Lsl
  | `Lsr
  | `Lt
  | `Mod
  | `Mult
  | `NotEqual
  | `Or
  | `Sub
  | `Xor ]
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
  | Local of (bind list * expr)
  | Null
  | Number of float
  | Object of objinside
  | ObjectSeq of (expr * objinside)
  | Select of (expr * id)
  | Self
  | Super
  | String of string
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
  type binop =
    [ `Add
    | `And
    | `Div
    | `Ge
    | `Gt
    | `Land
    | `Le
    | `Lor
    | `Lsl
    | `Lsr
    | `Lt
    | `Mult
    | `Or
    | `Sub
    | `Xor ]
  [@@deriving show]

  type expr =
    | Array of expr list
    | ArrayIndex of (expr * expr)
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
    | Self
    | String of string
    | Super
    | True
    | Unary of (unaryop * expr)
    | Var of id

  and id = string [@@deriving show]
end

let gensym =
  let i = ref 0 in
  fun () ->
    i := !i + 1;
    "$v" ^ string_of_int !i

let rec desugar_expr b = function
  | Array xs -> Core.Array (xs |> List.map (desugar_expr b))
  | ArrayFor (e, forspec, compspec) ->
      desugar_arrcomp e b (Forspec forspec :: compspec)
  | ArrayIndex (e1, e2) -> Core.ArrayIndex (desugar_expr b e1, desugar_expr b e2)
  | ArraySlice (e, e', e'', e''') ->
      let e' = e' |> Option.value ~default:Null in
      let e'' = e'' |> Option.value ~default:Null in
      let e''' = e''' |> Option.value ~default:Null in
      desugar_expr b
        (Call (Select (Var "std", "slice"), ([ e; e'; e''; e''' ], []), false))
  | Assert ((e, None), e') ->
      desugar_expr b (Assert ((e, Some (String "Assertion failed")), e'))
  | Assert ((e, Some e'), e'') -> desugar_expr b (If (e, e'', Some (Error e')))
  | Binary (e, `NotEqual, e') ->
      desugar_expr b (Unary (Not, Binary (e, `Equal, e')))
  | Binary (e, `Equal, e') ->
      desugar_expr b
        (Call (Select (Var "std", "equals"), ([ e; e' ], []), false))
  | Binary (e, `Mod, e') ->
      desugar_expr b (Call (Select (Var "std", "mod"), ([ e; e' ], []), false))
  | Binary (e, `In, e') ->
      desugar_expr b
        (Call (Select (Var "std", "objectHasEx"), ([ e'; e; True ], []), false))
  | Binary (e1, (#Core.binop as op), e2) ->
      Core.Binary (desugar_expr b e1, op, desugar_expr b e2)
  | Call (e, (xs, ys), _) ->
      Core.Call
        ( desugar_expr b e,
          xs |> List.map (desugar_expr b),
          ys |> List.map (fun (id, y) -> (id, desugar_expr b y)) )
  | Dollar -> Var "$"
  | Error e -> Core.Error (desugar_expr b e)
  | False -> Core.False
  | Function (params, e) ->
      Core.Function (params |> List.map (desugar_param b), desugar_expr b e)
  | If (e, e', None) -> Core.If (desugar_expr b e, desugar_expr b e', Null)
  | If (e, e', Some e'') ->
      Core.If (desugar_expr b e, desugar_expr b e', desugar_expr b e'')
  | Import _ -> assert false
  | Importbin _ -> assert false
  | Importstr _ -> assert false
  | Local (binds, e) ->
      let binds = binds |> List.map (desugar_bind b) in
      let e = desugar_expr b e in
      Core.Local (binds, e)
  | Null -> Core.Null
  | Number v -> Core.Number v
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
  | ObjectSeq (e, objinside) ->
      desugar_expr b (Binary (e, `Add, Object objinside))
  | Select (e, id) -> Core.ArrayIndex (desugar_expr b e, String id)
  | Self -> Core.Self
  | Super -> Core.Super
  | String s -> Core.String s
  | True -> Core.True
  | Unary (op, e) -> Core.Unary (op, desugar_expr b e)
  | Var s -> Core.Var s

and desugar_arrcomp e b compspec =
  let rec aux e = function
    | [] -> Array [ e ]
    | Ifspec e' :: compspec -> If (e', aux e compspec, Some (Array []))
    | Forspec (x, e') :: compspec ->
        let arr = gensym () in
        let i = gensym () in
        Local
          ( [ Bind (arr, e') ],
            Call
              ( Select (Var "std", "join"),
                ( [
                    Array [];
                    Call
                      ( Select (Var "std", "makeArray"),
                        ( [
                            Call
                              ( Select (Var "std", "length"),
                                ([ Var arr ], []),
                                false );
                            Function
                              ( [ (i, None) ],
                                Local
                                  ( [ Bind (x, ArrayIndex (Var arr, Var i)) ],
                                    aux e compspec ) );
                          ],
                          [] ),
                        false );
                  ],
                  [] ),
                false ) )
  in
  desugar_expr b (aux e compspec)

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
      let e''' =
        If
          ( Binary (e'', `In, Super),
            Binary (ArrayIndex (Super, e''), `Add, e'),
            Some e' )
      in
      desugar_field binds b (Field (FieldnameExpr e, false, h, e'''))

and desugar_bind b = function
  | Bind (id, e) -> (id, desugar_expr b e)
  | BindFunc (id, params, e) -> (id, desugar_expr b (Function (params, e)))

and desugar_param b = function
  | id, None -> (id, Core.Error (String "Parameter not bound"))
  | id, Some e -> (id, desugar_expr b e)
