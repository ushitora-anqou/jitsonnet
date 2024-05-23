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

let gensym ?(suffix = "") () =
  gensym_i := !gensym_i + 1;
  "$v" ^ string_of_int !gensym_i ^ suffix

let rec desugar_expr std b = function
  | Array xs -> Core.Array (xs |> List.map (desugar_expr std b))
  | ArrayFor (e, forspec, compspec) ->
      desugar_arrcomp std e b (Forspec forspec :: compspec)
  | ArrayIndex (e1, e2) ->
      Core.ArrayIndex (desugar_expr std b e1, desugar_expr std b e2)
  | ArraySlice (e, e', e'', e''') ->
      let e' = e' |> Option.value ~default:Null in
      let e'' = e'' |> Option.value ~default:Null in
      let e''' = e''' |> Option.value ~default:Null in
      desugar_expr std b
        (Call (Select (Var std, "slice"), ([ e; e'; e''; e''' ], []), false))
  | Assert ((e, None), e') ->
      desugar_expr std b (Assert ((e, Some (String "Assertion failed")), e'))
  | Assert ((e, Some e'), e'') ->
      desugar_expr std b (If (e, e'', Some (Error e')))
  | Binary (e, `NotEqual, e') ->
      desugar_expr std b (Unary (Not, Binary (e, `Equal, e')))
  | Binary (e, `Equal, e') ->
      desugar_expr std b
        (Call (Select (Var std, "equals"), ([ e; e' ], []), false))
  | Binary (e, `Mod, e') ->
      desugar_expr std b
        (Call (Select (Var std, "mod"), ([ e; e' ], []), false))
  | Binary (e, `In, e') ->
      desugar_expr std b
        (Call (Select (Var std, "objectHasEx"), ([ e'; e; True ], []), false))
  | Binary (e1, (#Core.binop as op), e2) ->
      Core.Binary (desugar_expr std b e1, op, desugar_expr std b e2)
  | Call (e, (xs, ys), _) ->
      Core.Call
        ( desugar_expr std b e,
          xs |> List.map (desugar_expr std b),
          ys |> List.map (fun (id, y) -> (id, desugar_expr std b y)) )
  | Dollar -> Var "$"
  | Error e -> Core.Error (desugar_expr std b e)
  | False -> Core.False
  | Function (params, e) ->
      Core.Function
        (params |> List.map (desugar_param std b), desugar_expr std b e)
  | If (e, e', None) ->
      Core.If (desugar_expr std b e, desugar_expr std b e', Null)
  | If (e, e', Some e'') ->
      Core.If
        (desugar_expr std b e, desugar_expr std b e', desugar_expr std b e'')
  | Import s -> Core.Import s
  | Importbin s -> Core.Importbin s
  | Importstr s -> Core.Importstr s
  | InSuper e -> Core.InSuper (desugar_expr std b e)
  | Local (binds, e) ->
      let binds = binds |> List.map (desugar_bind std b) in
      let e = desugar_expr std b e in
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
        |> List.map (desugar_bind std b)
      in
      let assrts =
        members
        |> List.filter_map (function
             | MemberAssert assrt -> Some (desugar_assert std [] assrt)
             | _ -> None)
      in
      let fields =
        members
        |> List.filter_map (function
             | MemberField field -> Some (desugar_field std [] b field)
             | _ -> None)
      in
      let obj = Core.Object { binds; assrts; fields } in
      if b then obj else obj
  | Object (ObjectFor ([], Var x1, e1, [], (x2, e2), [])) when x1 = x2 ->
      (* Optimized desugaring *)
      Core.ObjectFor
        ( Var x1,
          (let e1 = desugar_expr std true e1 in
           if b then e1 else Local ([ ("$", Self) ], e1)),
          x2,
          desugar_expr std b e2 )
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
        ( desugar_expr std b (Local (binds_xs, ef)),
          (let e2 =
             desugar_expr std true (Local (binds_xs @ binds @ binds', ebody))
           in
           if b then e2 else Local ([ ("$", Self) ], e2)),
          arr,
          desugar_expr std b (ArrayFor (Array vars_xs, forspec, compspec)) )
  | ObjectSeq (e, objinside) ->
      desugar_expr std b (Binary (e, `Add, Object objinside))
  | Select (e, id) -> Core.ArrayIndex (desugar_expr std b e, String id)
  | Self -> Core.Self
  | String s -> Core.String s
  | SuperIndex e -> Core.SuperIndex (desugar_expr std b e)
  | True -> Core.True
  | Unary (op, e) -> Core.Unary (op, desugar_expr std b e)
  | Var s -> Core.Var s

and desugar_arrcomp std e b compspec =
  let rec aux e = function
    | [] -> Array [ e ]
    | Ifspec e' :: compspec -> If (e', aux e compspec, Some (Array []))
    | Forspec (x, e') :: compspec ->
        let arr = gensym () in
        let i = gensym () in
        Local
          ( [ Bind (arr, e') ],
            Call
              ( Select (Var std, "join"),
                ( [
                    Array [];
                    Call
                      ( Select (Var std, "makeArray"),
                        ( [
                            Call
                              ( Select (Var std, "length"),
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
  desugar_expr std b (aux e compspec)

and desugar_assert std binds = function
  | e, None -> desugar_assert std binds (e, Some (String "Assertion failed"))
  | e, Some e' ->
      assert (binds = []);
      desugar_expr std true (If (e, Null, Some (Error e')))

and desugar_field std binds b = function
  | Field ((FieldnameID id | FieldnameString id), plus, h, e) ->
      desugar_field std binds b (Field (FieldnameExpr (String id), plus, h, e))
  | FieldFunc ((FieldnameID id | FieldnameString id), params, h, e) ->
      desugar_field std binds b
        (FieldFunc (FieldnameExpr (String id), params, h, e))
  | Field (FieldnameExpr e, b, h, e') ->
      assert (binds = []);
      (desugar_expr std b e, b, h, desugar_expr std true e')
  | FieldFunc (FieldnameExpr e, params, h, e') ->
      desugar_field std binds b
        (Field (FieldnameExpr e, false, h, Function (params, e')))
(*
  | Field (FieldnameExpr e, true, h, e') ->
      (* FIXME
         let e'' =
           substitute [ (Var "$outerself", Self); (Var "$outersuper", Super) ] e
         in *)
      let e'' = e in
      let e''' = If (InSuper e'', Binary (SuperIndex e'', `Add, e'), Some e') in
      desugar_field std binds b (Field (FieldnameExpr e, false, h, e'''))
*)

and desugar_bind std b = function
  | Bind (id, e) -> (id, desugar_expr std b e)
  | BindFunc (id, params, e) -> (id, desugar_expr std b (Function (params, e)))

and desugar_param std b = function
  | id, None -> (id, None)
  | id, Some e -> (id, Some (desugar_expr std b e))

let desugar ?(is_stdjsonnet = false) { expr } =
  desugar_expr (if is_stdjsonnet then "std" else "$std") false expr

module StringSet = Set.Make (String)

let with_binds ?(as_is = false) env ids f =
  ids
  |> List.iter (fun id ->
         Hashtbl.add env id (if as_is then id else gensym ~suffix:id ()));
  Fun.protect
    ~finally:(fun () -> ids |> List.iter (fun id -> Hashtbl.remove env id))
    f

let alpha_conv ?(is_stdjsonnet = false) (e : Core.expr) =
  let open Core in
  let rec aux env = function
    | ( False | Import _ | Importbin _ | Importstr _ | Null | Number _ | Self
      | String _ | True ) as x ->
        x
    | Var "std" when is_stdjsonnet -> Var "std"
    | Var name -> Var (Hashtbl.find env name)
    | Array xs -> Array (List.map (aux env) xs)
    | ArrayIndex (e1, e2) -> ArrayIndex (aux env e1, aux env e2)
    | Binary (e1, op, e2) -> Binary (aux env e1, op, aux env e2)
    | Call (e1, xs, ys) ->
        Call
          ( aux env e1,
            List.map (aux env) xs,
            List.map (fun (x, y) -> (x, aux env y)) ys )
    | Error e -> Error (aux env e)
    | Unary (op, e) -> Unary (op, aux env e)
    | Function (xs, e) ->
        with_binds ~as_is:true env (List.map fst xs) @@ fun () ->
        Function
          (List.map (fun (x, y) -> (x, Option.map (aux env) y)) xs, aux env e)
    | Local (xs, e) ->
        with_binds env (List.map fst xs) @@ fun () ->
        Local
          ( List.map (fun (x, y) -> (Hashtbl.find env x, aux env y)) xs,
            aux env e )
    | If (e1, e2, e3) -> If (aux env e1, aux env e2, aux env e3)
    | InSuper e -> InSuper (aux env e)
    | ObjectFor (e1, e2, x, e3) ->
        let e3 = aux env e3 in
        with_binds env [ x ] @@ fun () ->
        let e1 = aux env e1 in
        let ids = [ "self"; "super" ] in
        with_binds env ids @@ fun () ->
        let e2 = aux env e2 in
        ObjectFor (e1, e2, Hashtbl.find env x, e3)
    | Object { binds; assrts; fields } ->
        let fields =
          List.map (fun (e1, b, h, e2) -> (aux env e1, b, h, e2)) fields
        in
        with_binds env
          ("self" :: "super"
          :: List.filter_map
               (function
                 | "std", _ when is_stdjsonnet -> None | id, _ -> Some id)
               binds)
        @@ fun () ->
        Object
          {
            binds =
              List.map
                (function
                  | "std", x when is_stdjsonnet -> ("std", aux env x)
                  | id, x -> (Hashtbl.find env id, aux env x))
                binds;
            assrts = List.map (aux env) assrts;
            fields =
              List.map (fun (e1, b, h, e2) -> (e1, b, h, aux env e2)) fields;
          }
    | SuperIndex e -> SuperIndex (aux env e)
  in
  aux
    ((if is_stdjsonnet then [] else [ ("std", "$std"); ("$std", "$std") ])
    |> List.to_seq |> Hashtbl.of_seq)
    e

(* freevars don't include self and super, but may include $. *)
let freevars e =
  let rec aux = function
    | Core.False | Import _ | Importbin _ | Importstr _ | Null | Number _
    | String _ | True ->
        StringSet.empty
    | Var x -> StringSet.singleton x
    | Self -> StringSet.singleton "self"
    | Array xs ->
        List.fold_left
          (fun acc x -> StringSet.union acc (aux x))
          StringSet.empty xs
    | ArrayIndex (e1, e2) | Binary (e1, _, e2) ->
        StringSet.union (aux e1) (aux e2)
    | Call (e1, xs, ys) ->
        let acc = aux e1 in
        let acc =
          xs |> List.fold_left (fun acc x -> StringSet.union acc (aux x)) acc
        in
        let acc =
          ys
          |> List.fold_left (fun acc (_, y) -> StringSet.union acc (aux y)) acc
        in
        acc
    | Error e | Unary (_, e) -> aux e
    | InSuper e | SuperIndex e -> StringSet.add "super" (aux e)
    | Function (xs, e) ->
        let acc =
          xs
          |> List.fold_left
               (fun acc (_, x) ->
                 match x with
                 | None -> acc
                 | Some x -> StringSet.union acc (aux x))
               StringSet.empty
        in
        let acc = StringSet.union acc (aux e) in
        List.fold_left (fun acc (x, _) -> StringSet.remove x acc) acc xs
    | Local (xs, e) ->
        let acc =
          xs
          |> List.fold_left
               (fun acc (_, x) -> StringSet.union acc (aux x))
               StringSet.empty
        in
        let acc = StringSet.union acc (aux e) in
        List.fold_left (fun acc (x, _) -> StringSet.remove x acc) acc xs
    | If (e1, e2, e3) ->
        let acc = aux e1 in
        let acc = StringSet.union acc (aux e2) in
        let acc = StringSet.union acc (aux e3) in
        acc
    | ObjectFor (e1, e2, x, e3) ->
        let acc = aux e2 in
        let acc = StringSet.remove "self" acc in
        let acc = StringSet.remove "super" acc in
        let acc = StringSet.union acc (aux e1) in
        let acc = StringSet.remove x acc in
        let acc = StringSet.union acc (aux e3) in
        acc
    | Object { binds; assrts; fields } ->
        let acc =
          binds
          |> List.fold_left
               (fun acc (_, x) -> StringSet.union acc (aux x))
               StringSet.empty
        in
        let acc =
          assrts
          |> List.fold_left (fun acc x -> StringSet.union acc (aux x)) acc
        in
        let acc =
          fields
          |> List.fold_left
               (fun acc (_, _, _, e2) -> StringSet.union acc (aux e2))
               acc
        in
        let acc =
          List.fold_left (fun acc (x, _) -> StringSet.remove x acc) acc binds
        in
        let acc = StringSet.remove "self" acc in
        let acc = StringSet.remove "super" acc in
        let acc =
          fields
          |> List.fold_left
               (fun acc (e1, _, _, _) -> StringSet.union acc (aux e1))
               acc
        in
        acc
  in
  aux e

let separate_floating_binds defvars floating_binds =
  let rec loop (floating_binds, fixed_binds) =
    let defvars = StringSet.of_list (defvars @ List.map fst fixed_binds) in
    let floating_binds', fixed_binds' =
      floating_binds
      |> List.partition (fun (_, e) -> StringSet.disjoint defvars (freevars e))
    in
    if List.length floating_binds = List.length floating_binds' then
      (floating_binds, fixed_binds)
    else loop (floating_binds', fixed_binds @ fixed_binds')
  in
  loop (floating_binds, [])

(* float_let_binds requires e is alpha-converted *)
let float_let_binds (e : Core.expr) =
  let local (binds, body) =
    match binds with [] -> body | _ -> Core.Local (binds, body)
  in
  let rec aux = function
    | ( Core.False | Import _ | Importbin _ | Importstr _ | Null | Number _
      | Self | String _ | True | Var _ ) as x ->
        ([], x)
    | Array xs ->
        let floating_binds, xs = xs |> List.map aux |> List.split in
        (List.flatten floating_binds, Array xs)
    | ArrayIndex (e1, e2) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, e2 = aux e2 in
        (floating_binds1 @ floating_binds2, ArrayIndex (e1, e2))
    | Binary (e1, op, e2) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, e2 = aux e2 in
        (floating_binds1 @ floating_binds2, Binary (e1, op, e2))
    | Call (e1, xs, ys) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, xs = xs |> List.map aux |> List.split in
        let floating_binds3, ys =
          ys
          |> List.map (fun (id, y) ->
                 let floating_binds, y = aux y in
                 (floating_binds, (id, y)))
          |> List.split
        in
        ( List.flatten (floating_binds1 :: (floating_binds2 @ floating_binds3)),
          Call (e1, xs, ys) )
    | Error e ->
        let floating_binds, e = aux e in
        (floating_binds, Error e)
    | Function (xs, body) ->
        (* FIXME: optimize default arguments *)
        let floating_binds, body = aux body in
        let floating_binds, fixed_binds =
          separate_floating_binds (List.map fst xs) floating_binds
        in
        (floating_binds, Function (xs, local (fixed_binds, body)))
    | If (e1, e2, e3) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, e2 = aux e2 in
        let floating_binds3, e3 = aux e3 in
        (floating_binds1 @ floating_binds2 @ floating_binds3, If (e1, e2, e3))
    | InSuper e ->
        let floating_binds, e = aux e in
        (floating_binds, InSuper e)
    | Local (binds, body) ->
        let floating_binds, body = aux body in
        let floating_binds =
          binds
          |> List.fold_left
               (fun acc (id, e) ->
                 let floating_binds', e' = aux e in
                 (id, e') :: (floating_binds' @ acc))
               floating_binds
        in
        (floating_binds, body)
    | Object { binds; assrts; fields } ->
        let floating_binds1, binds =
          binds
          |> List.map (fun (id, e) ->
                 let floating_binds, e = aux e in
                 (floating_binds, (id, e)))
          |> List.split
        in
        let floating_binds1 = List.flatten floating_binds1 in

        let floating_binds2, assrts = assrts |> List.map aux |> List.split in
        let floating_binds2 = List.flatten floating_binds2 in

        let floating_binds31, (floating_binds32, fields) =
          fields
          |> List.map (fun (e1, b, h, e2) ->
                 let floating_binds31, e1 = aux e1 in
                 let floating_binds32, e2 = aux e2 in
                 (floating_binds31, (floating_binds32, (e1, b, h, e2))))
          |> List.split
          |> fun (x, y) -> (x, List.split y)
        in
        let floating_binds31 = List.flatten floating_binds31 in
        let floating_binds32 = List.flatten floating_binds32 in

        let floating_binds, fixed_binds =
          separate_floating_binds
            ("self" :: "super" :: List.map fst binds)
            (floating_binds1 @ floating_binds2 @ floating_binds32)
        in

        ( floating_binds @ floating_binds31,
          Object { binds = fixed_binds @ binds; assrts; fields } )
    | ObjectFor (e1, e2, x, e3) ->
        (* FIXME: move binds outside of e1, e2, e3 *)
        let e1 = local (aux e1) in
        let e2 = local (aux e2) in
        let e3 = local (aux e3) in
        ([], ObjectFor (e1, e2, x, e3))
    | SuperIndex e ->
        let floating_binds, e = aux e in
        (floating_binds, SuperIndex e)
    | Unary (op, e) ->
        let floating_binds, e = aux e in
        (floating_binds, Unary (op, e))
  in
  local (aux e)
