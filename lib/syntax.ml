type unaryop = Lnot | Neg | Not | Pos [@@deriving show]

type binop =
  [ `Add
  | `And (* && *)
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
    [ `Add (* + *)
    | `And (* && *)
    | `Div (* / *)
    | `Ge (* >= *)
    | `Gt (* > *)
    | `Land (* & *)
    | `Le (* <= *)
    | `Lor (* | *)
    | `Lsl (* << *)
    | `Lsr (* >> *)
    | `Lt (* < *)
    | `Mult (* * *)
    | `Or (* || *)
    | `Sub (* - *)
    | `Xor (* ^ *) ]
  [@@deriving show]

  type expr =
    | Array of expr list
    | ArrayIndex of (expr * expr)
    | Binary of (expr * binop * expr)
    | Call of (expr * expr list * (id * expr) list)
    | Error of expr
    | False
    | Function of ((id * expr option) list * expr)
    | If of (expr * expr * expr)
    | Import of string
    | Importbin of string
    | Importstr of string
    | InSuper of expr
    | Local of ((id * expr) list * expr)
    | Null
    | Number of float
    | Object of {
        binds : (id * expr) list;
        assrts : expr list;
        fields : (expr * bool * h * expr) list;
      }
    | ObjectFor of (expr * expr * id * expr)
    | Self
    | String of string
    | SuperIndex of expr
    | True
    | Unary of (unaryop * expr)
    | Var of id

  and id = string [@@deriving show]

  let map f root =
    let rec aux node =
      match node with
      | False | Import _ | Importbin _ | Importstr _ | Null | Number _ | Self
      | String _ | True | Var _ ->
          f node
      | Array xs -> f (Array (List.map aux xs))
      | ArrayIndex (e1, e2) -> f (ArrayIndex (aux e1, aux e2))
      | Binary (e1, op, e2) -> f (Binary (aux e1, op, aux e2))
      | Call (e1, xs, ys) ->
          f
            (Call
               (aux e1, List.map aux xs, List.map (fun (x, y) -> (x, aux y)) ys))
      | Error e -> f (Error (aux e))
      | Unary (op, e) -> f (Unary (op, aux e))
      | Function (xs, e) ->
          f
            (Function
               (List.map (fun (x, y) -> (x, y |> Option.map aux)) xs, aux e))
      | Local (xs, e) ->
          f (Local (List.map (fun (x, y) -> (x, aux y)) xs, aux e))
      | If (e1, e2, e3) -> f (If (aux e1, aux e2, aux e3))
      | InSuper e -> f (InSuper (aux e))
      | ObjectFor (e1, e2, x, e3) -> f (ObjectFor (aux e1, aux e2, x, aux e3))
      | Object { binds; assrts; fields } ->
          f
            (Object
               {
                 binds = binds |> List.map (fun (id, x) -> (id, aux x));
                 assrts = assrts |> List.map aux;
                 fields =
                   fields
                   |> List.map (fun (e1, b, h, e2) -> (aux e1, b, h, aux e2));
               })
      | SuperIndex e -> f (SuperIndex (aux e))
    in
    aux root

  let fold f a root =
    let rec aux acc node =
      match node with
      | False | Import _ | Importbin _ | Importstr _ | Null | Number _ | Self
      | String _ | True | Var _ ->
          f acc node
      | Array xs ->
          let acc = xs |> List.fold_left aux acc in
          f acc node
      | ArrayIndex (e1, e2) | Binary (e1, _, e2) ->
          let acc = aux acc e1 in
          let acc = aux acc e2 in
          f acc node
      | Call (e1, xs, ys) ->
          let acc = aux acc e1 in
          let acc = xs |> List.fold_left aux acc in
          let acc = ys |> List.fold_left (fun acc (_, y) -> aux acc y) acc in
          f acc node
      | Error e | InSuper e | SuperIndex e | Unary (_, e) ->
          let acc = aux acc e in
          f acc node
      | Function (xs, e) ->
          let acc =
            xs
            |> List.fold_left
                 (fun acc (_, x) ->
                   match x with None -> acc | Some x -> aux acc x)
                 acc
          in
          let acc = aux acc e in
          f acc node
      | Local (xs, e) ->
          let acc = xs |> List.fold_left (fun acc (_, x) -> aux acc x) acc in
          let acc = aux acc e in
          f acc node
      | If (e1, e2, e3) | ObjectFor (e1, e2, _, e3) ->
          let acc = aux acc e1 in
          let acc = aux acc e2 in
          let acc = aux acc e3 in
          f acc node
      | Object { binds; assrts; fields } ->
          let acc = binds |> List.fold_left (fun acc (_, x) -> aux acc x) acc in
          let acc = assrts |> List.fold_left aux acc in
          let acc =
            fields
            |> List.fold_left
                 (fun acc (e1, _, _, e2) -> aux (aux acc e1) e2)
                 acc
          in
          f acc node
    in
    aux a root
end

let gensym_i = ref 0
let reset_gensym_i () = gensym_i := 0 (* for tests *)

let gensym () =
  gensym_i := !gensym_i + 1;
  "$v" ^ string_of_int !gensym_i

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
  | Import s -> Core.Import s
  | Importbin s -> Core.Importbin s
  | Importstr s -> Core.Importstr s
  | InSuper e -> Core.InSuper (desugar_expr b e)
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
      let binds =
        (if b then binds else Bind ("$", Self) :: binds)
        |> List.map (desugar_bind b)
      in
      let assrts =
        members
        |> List.filter_map (function
             | MemberAssert assrt -> Some (desugar_assert [] assrt)
             | _ -> None)
      in
      let fields =
        members
        |> List.filter_map (function
             | MemberField field -> Some (desugar_field [] b field)
             | _ -> None)
      in
      let obj = Core.Object { binds; assrts; fields } in
      if b then obj else obj
  | Object (ObjectFor ([], Var x1, e1, [], (x2, e2), [])) when x1 = x2 ->
      (* Optimized desugaring *)
      Core.ObjectFor
        ( Var x1,
          (let e1 = desugar_expr true e1 in
           if b then e1 else Local ([ ("$", Self) ], e1)),
          x2,
          desugar_expr b e2 )
  | Object (ObjectFor (binds, ef, ebody, binds', forspec, compspec)) ->
      let arr = gensym () in
      let xs =
        fst forspec
        :: (compspec
           |> List.filter_map (function
                | Ifspec _ -> None
                | Forspec (id, _) -> Some id))
      in
      let binds_xs =
        xs
        |> List.mapi (fun i x ->
               Bind (x, ArrayIndex (Var arr, Number (float_of_int i))))
      in
      let vars_xs = xs |> List.map (fun x -> Var x) in
      Core.ObjectFor
        ( desugar_expr b (Local (binds_xs, ef)),
          (let e2 =
             desugar_expr true (Local (binds_xs @ binds @ binds', ebody))
           in
           if b then e2 else Local ([ ("$", Self) ], e2)),
          arr,
          desugar_expr b (ArrayFor (Array vars_xs, forspec, compspec)) )
  | ObjectSeq (e, objinside) ->
      desugar_expr b (Binary (e, `Add, Object objinside))
  | Select (e, id) -> Core.ArrayIndex (desugar_expr b e, String id)
  | Self -> Core.Self
  | String s -> Core.String s
  | SuperIndex e -> Core.SuperIndex (desugar_expr b e)
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
      assert (binds = []);
      desugar_expr true (If (e, Null, Some (Error e')))

and desugar_field binds b = function
  | Field ((FieldnameID id | FieldnameString id), plus, h, e) ->
      desugar_field binds b (Field (FieldnameExpr (String id), plus, h, e))
  | FieldFunc ((FieldnameID id | FieldnameString id), params, h, e) ->
      desugar_field binds b
        (FieldFunc (FieldnameExpr (String id), params, h, e))
  | Field (FieldnameExpr e, b, h, e') ->
      assert (binds = []);
      (desugar_expr b e, b, h, desugar_expr true e')
  | FieldFunc (FieldnameExpr e, params, h, e') ->
      desugar_field binds b
        (Field (FieldnameExpr e, false, h, Function (params, e')))
(*
  | Field (FieldnameExpr e, true, h, e') ->
      (* FIXME
         let e'' =
           substitute [ (Var "$outerself", Self); (Var "$outersuper", Super) ] e
         in *)
      let e'' = e in
      let e''' = If (InSuper e'', Binary (SuperIndex e'', `Add, e'), Some e') in
      desugar_field binds b (Field (FieldnameExpr e, false, h, e'''))
*)

and desugar_bind b = function
  | Bind (id, e) -> (id, desugar_expr b e)
  | BindFunc (id, params, e) -> (id, desugar_expr b (Function (params, e)))

and desugar_param b = function
  | id, None -> (id, None)
  | id, Some e -> (id, Some (desugar_expr b e))

let desugar { expr } = desugar_expr false expr
