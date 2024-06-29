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

type position = { fname : string; line : int; column : int } [@@deriving show]
type location = { startpos : position; endpos : position } [@@deriving show]
type 'a with_location = { v : 'a; loc : location option } [@@deriving show]

type expr' =
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

and expr = expr' with_location
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
  | FieldFunc of (fieldname * (params * h * expr) with_location)

and h = H of int
and objlocal = bind
and compspec = compspec' list
and compspec' = Forspec of forspec | Ifspec of ifspec
and forspec = id * expr
and ifspec = expr

and fieldname =
  | FieldnameExpr of expr
  | FieldnameID of id with_location
  | FieldnameString of string with_location

and assert_ = (expr * expr option) with_location

and bind =
  | Bind of (id * expr)
  | BindFunc of (id * (params * expr) with_location)

and args = expr list * (id * expr) list
and params = param list
and param = id * expr option [@@deriving show]

type program = { expr : expr } [@@deriving show]

exception General_parse_error of string

module Core = struct
  type nonrec 'a with_location = 'a with_location = {
    v : 'a;
    loc : location option;
  }

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

  type expr' =
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

  and expr = expr' with_location
  and id = string [@@deriving show]

  let map f root =
    let rec aux node =
      let with_ v = { node with v } in
      match node.v with
      | False | Import _ | Importbin _ | Importstr _ | Null | Number _ | Self
      | String _ | True | Var _ ->
          f node
      | Array xs -> f (with_ @@ Array (List.map aux xs))
      | ArrayIndex (e1, e2) -> f (with_ @@ ArrayIndex (aux e1, aux e2))
      | Binary (e1, op, e2) -> f (with_ @@ Binary (aux e1, op, aux e2))
      | Call (e1, xs, ys) ->
          f
            (with_
            @@ Call
                 ( aux e1,
                   List.map aux xs,
                   List.map (fun (x, y) -> (x, aux y)) ys ))
      | Error e -> f (with_ @@ Error (aux e))
      | Unary (op, e) -> f (with_ @@ Unary (op, aux e))
      | Function (xs, e) ->
          f
            (with_
            @@ Function
                 (List.map (fun (x, y) -> (x, y |> Option.map aux)) xs, aux e))
      | Local (xs, e) ->
          f (with_ @@ Local (List.map (fun (x, y) -> (x, aux y)) xs, aux e))
      | If (e1, e2, e3) -> f (with_ @@ If (aux e1, aux e2, aux e3))
      | InSuper e -> f (with_ @@ InSuper (aux e))
      | ObjectFor (e1, e2, x, e3) ->
          f (with_ @@ ObjectFor (aux e1, aux e2, x, aux e3))
      | Object { binds; assrts; fields } ->
          f
            (with_
            @@ Object
                 {
                   binds = binds |> List.map (fun (id, x) -> (id, aux x));
                   assrts = assrts |> List.map aux;
                   fields =
                     fields
                     |> List.map (fun (e1, b, h, e2) -> (aux e1, b, h, aux e2));
                 })
      | SuperIndex e -> f (with_ @@ SuperIndex (aux e))
    in
    aux root

  let fold f a root =
    let rec aux acc node =
      match node.v with
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

let rec desugar_expr std b e0 =
  let with_ v = { e0 with v } in
  match e0.v with
  | Array xs -> with_ (Core.Array (xs |> List.map (desugar_expr std b)))
  | ArrayFor (e, forspec, compspec) ->
      desugar_arrcomp std e b (Forspec forspec :: compspec)
  | ArrayIndex (e1, e2) ->
      with_ (Core.ArrayIndex (desugar_expr std b e1, desugar_expr std b e2))
  | ArraySlice (e, e', e'', e''') ->
      let e' = e' |> Option.value ~default:(with_ Null) in
      let e'' = e'' |> Option.value ~default:(with_ Null) in
      let e''' = e''' |> Option.value ~default:(with_ Null) in
      desugar_expr std b
        (with_
           (Call
              ( with_ (Select (with_ (Var std), "slice")),
                ([ e; e'; e''; e''' ], []),
                false )))
  | Assert ({ v = e, None; _ }, e') ->
      desugar_expr std b
        (with_
           (Assert
              ({ e with v = (e, Some (with_ (String "Assertion failed"))) }, e')))
  | Assert (({ v = e, Some e'; _ } as e1), e'') ->
      desugar_expr std b (with_ (If (e, e'', Some { e1 with v = Error e' })))
  | Binary (e, `NotEqual, e') ->
      desugar_expr std b (with_ (Unary (Not, with_ (Binary (e, `Equal, e')))))
  | Binary (e, `Equal, e') ->
      desugar_expr std b
        (with_
           (Call
              ( with_ (Select (with_ (Var std), "equals")),
                ([ e; e' ], []),
                false )))
  | Binary (e, `Mod, e') ->
      desugar_expr std b
        (with_
           (Call
              (with_ (Select (with_ (Var std), "mod")), ([ e; e' ], []), false)))
  | Binary (e, `In, e') ->
      desugar_expr std b
        (with_
           (Call
              ( with_ (Select (with_ (Var std), "objectHasEx")),
                ([ e'; e; with_ True ], []),
                false )))
  | Binary (e1, (#Core.binop as op), e2) ->
      with_ (Core.Binary (desugar_expr std b e1, op, desugar_expr std b e2))
  | Call (e, (xs, ys), _) ->
      with_
        (Core.Call
           ( desugar_expr std b e,
             xs |> List.map (desugar_expr std b),
             ys |> List.map (fun (id, y) -> (id, desugar_expr std b y)) ))
  | Dollar -> with_ (Core.Var "$")
  | Error e -> with_ (Core.Error (desugar_expr std b e))
  | False -> with_ Core.False
  | Function (params, e) ->
      with_
        (Core.Function
           (params |> List.map (desugar_param std b), desugar_expr std b e))
  | If (e, e', None) ->
      with_
        (Core.If (desugar_expr std b e, desugar_expr std b e', with_ Core.Null))
  | If (e, e', Some e'') ->
      with_
        (Core.If
           (desugar_expr std b e, desugar_expr std b e', desugar_expr std b e''))
  | Import s -> with_ (Core.Import s)
  | Importbin s -> with_ (Core.Importbin s)
  | Importstr s -> with_ (Core.Importstr s)
  | InSuper e -> with_ (Core.InSuper (desugar_expr std b e))
  | Local (binds, e) ->
      let binds = binds |> List.map (desugar_bind std b) in
      let e = desugar_expr std b e in
      with_ (Core.Local (binds, e))
  | Null -> with_ Core.Null
  | Number v -> with_ (Core.Number v)
  | Object (ObjectMemberList members) ->
      let binds =
        members
        |> List.filter_map (function MemberObjlocal l -> Some l | _ -> None)
      in
      let binds =
        (if b then binds else Bind ("$", with_ Self) :: binds)
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
      with_ (Core.Object { binds; assrts; fields })
  | Object (ObjectFor ([], { v = Var x1; _ }, e1, [], (x2, e2), []))
    when x1 = x2 ->
      (* Optimized desugaring *)
      with_
        (Core.ObjectFor
           ( with_ (Core.Var x1),
             (let e1 = desugar_expr std true e1 in
              if b then e1
              else with_ (Core.Local ([ ("$", with_ Core.Self) ], e1))),
             x2,
             desugar_expr std b e2 ))
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
               Bind
                 ( x,
                   with_
                     (ArrayIndex
                        (with_ (Var arr), with_ (Number (float_of_int i)))) ))
      in
      let vars_xs = xs |> List.map (fun x -> with_ (Var x)) in
      with_
        (Core.ObjectFor
           ( desugar_expr std b (with_ (Local (binds_xs, ef))),
             (let e2 =
                desugar_expr std true
                  (with_ (Local (binds_xs @ binds @ binds', ebody)))
              in
              if b then e2
              else with_ (Core.Local ([ ("$", with_ Core.Self) ], e2))),
             arr,
             desugar_expr std b
               (with_ (ArrayFor (with_ (Array vars_xs), forspec, compspec))) ))
  | ObjectSeq (e, objinside) ->
      desugar_expr std b (with_ (Binary (e, `Add, with_ (Object objinside))))
  | Select (e, id) ->
      with_ (Core.ArrayIndex (desugar_expr std b e, with_ (Core.String id)))
  | Self -> with_ Core.Self
  | String s -> with_ (Core.String s)
  | SuperIndex e -> with_ (Core.SuperIndex (desugar_expr std b e))
  | True -> with_ Core.True
  | Unary (op, e) -> with_ (Core.Unary (op, desugar_expr std b e))
  | Var s -> with_ (Core.Var s)

and desugar_arrcomp std e b compspec =
  let rec aux e v =
    match v with
    | [] -> { e with v = Array [ e ] }
    | Ifspec e' :: compspec ->
        let with_ v = { e' with v } in
        with_ (If (e', aux e compspec, Some (with_ (Array []))))
    | Forspec (x, e') :: compspec ->
        let with_ v = { e' with v } in
        let arr = gensym () in
        let i = gensym () in
        with_
          (Local
             ( [ Bind (arr, e') ],
               with_
                 (Call
                    ( with_ (Select (with_ (Var std), "join")),
                      ( [
                          with_ (Array []);
                          with_
                            (Call
                               ( with_ (Select (with_ (Var std), "makeArray")),
                                 ( [
                                     with_
                                       (Call
                                          ( with_
                                              (Select (with_ (Var std), "length")),
                                            ([ with_ (Var arr) ], []),
                                            false ));
                                     with_
                                       (Function
                                          ( [ (i, None) ],
                                            with_
                                              (Local
                                                 ( [
                                                     Bind
                                                       ( x,
                                                         with_
                                                           (ArrayIndex
                                                              ( with_ (Var arr),
                                                                with_ (Var i) ))
                                                       );
                                                   ],
                                                   aux e compspec )) ));
                                   ],
                                   [] ),
                                 false ));
                        ],
                        [] ),
                      false )) ))
  in
  desugar_expr std b (aux e compspec)

and desugar_assert std binds a =
  match a.v with
  | e, None ->
      desugar_assert std binds
        { a with v = (e, Some { a with v = String "Assertion failed" }) }
  | e, Some e' ->
      assert (binds = []);
      desugar_expr std true
        { a with v = If (e, { a with v = Null }, Some { a with v = Error e' }) }

and desugar_field std binds b = function
  | Field ((FieldnameID id | FieldnameString id), plus, h, e) ->
      desugar_field std binds b
        (Field (FieldnameExpr { id with v = String id.v }, plus, h, e))
  | FieldFunc ((FieldnameID id | FieldnameString id), paramsbody) ->
      desugar_field std binds b
        (FieldFunc (FieldnameExpr { id with v = String id.v }, paramsbody))
  | Field (FieldnameExpr e, b, h, e') ->
      assert (binds = []);
      (desugar_expr std b e, b, h, desugar_expr std true e')
  | FieldFunc (FieldnameExpr e, ({ v = params, h, e'; _ } as paramsbody)) ->
      desugar_field std binds b
        (Field
           ( FieldnameExpr e,
             false,
             h,
             { paramsbody with v = Function (params, e') } ))

and desugar_bind std b = function
  | Bind (id, e) -> (id, desugar_expr std b e)
  | BindFunc (id, paramsbody) ->
      (id, desugar_expr std b { paramsbody with v = Function paramsbody.v })

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

let replace_std ~is_stdjsonnet =
  let open Core in
  map (fun n ->
      match n.v with
      | Var ("std" | "$std") ->
          { n with v = Var (if is_stdjsonnet then "std" else "$std") }
      | _ -> n)

let alpha_conv ?(is_stdjsonnet = false) (e : Core.expr) =
  let open Core in
  let rec aux env e0 =
    let with_ v = { e0 with v } in
    match e0.v with
    | ( False | Import _ | Importbin _ | Importstr _ | Null | Number _ | Self
      | String _ | True ) as x ->
        with_ x
    | Var "std" when is_stdjsonnet -> with_ (Var "std")
    | Var name -> with_ (Var (Hashtbl.find env name))
    | Array xs -> with_ (Array (List.map (aux env) xs))
    | ArrayIndex (e1, e2) -> with_ (ArrayIndex (aux env e1, aux env e2))
    | Binary (e1, op, e2) -> with_ (Binary (aux env e1, op, aux env e2))
    | Call (e1, xs, ys) ->
        with_
          (Call
             ( aux env e1,
               List.map (aux env) xs,
               List.map (fun (x, y) -> (x, aux env y)) ys ))
    | Error e -> with_ (Error (aux env e))
    | Unary (op, e) -> with_ (Unary (op, aux env e))
    | Function (xs, e) ->
        with_binds ~as_is:true env (List.map fst xs) @@ fun () ->
        with_
          (Function
             (List.map (fun (x, y) -> (x, Option.map (aux env) y)) xs, aux env e))
    | Local (xs, e) ->
        with_binds env (List.map fst xs) @@ fun () ->
        with_
          (Local
             ( List.map (fun (x, y) -> (Hashtbl.find env x, aux env y)) xs,
               aux env e ))
    | If (e1, e2, e3) -> with_ (If (aux env e1, aux env e2, aux env e3))
    | InSuper e -> with_ (InSuper (aux env e))
    | ObjectFor (e1, e2, x, e3) ->
        let e3 = aux env e3 in
        with_binds env [ x ] @@ fun () ->
        let e1 = aux env e1 in
        let ids = [ "self"; "super" ] in
        with_binds env ids @@ fun () ->
        let e2 = aux env e2 in
        with_ (ObjectFor (e1, e2, Hashtbl.find env x, e3))
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
        with_
          (Object
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
             })
    | SuperIndex e -> with_ (SuperIndex (aux env e))
  in
  aux
    ((if is_stdjsonnet then [] else [ ("std", "$std"); ("$std", "$std") ])
    |> List.to_seq |> Hashtbl.of_seq)
    e

(* freevars don't include self and super, but may include $. *)
let freevars e =
  let rec aux e =
    match e.v with
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
    match binds with [] -> body.v | _ -> Core.Local (binds, body)
  in
  let rec aux e =
    let with_ v = { e with v } in
    match e.v with
    | ( Core.False | Import _ | Importbin _ | Importstr _ | Null | Number _
      | Self | String _ | True | Var _ ) as x ->
        ([], with_ x)
    | Array xs ->
        let floating_binds, xs = xs |> List.map aux |> List.split in
        (List.flatten floating_binds, with_ (Core.Array xs))
    | ArrayIndex (e1, e2) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, e2 = aux e2 in
        (floating_binds1 @ floating_binds2, with_ (Core.ArrayIndex (e1, e2)))
    | Binary (e1, op, e2) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, e2 = aux e2 in
        (floating_binds1 @ floating_binds2, with_ (Core.Binary (e1, op, e2)))
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
          with_ (Core.Call (e1, xs, ys)) )
    | Error e ->
        let floating_binds, e = aux e in
        (floating_binds, with_ (Core.Error e))
    | Function (xs, body) ->
        (* FIXME: optimize default arguments *)
        let floating_binds, body = aux body in
        let floating_binds, fixed_binds =
          separate_floating_binds (List.map fst xs) floating_binds
        in
        ( floating_binds,
          with_
            (Core.Function (xs, { body with v = local (fixed_binds, body) })) )
    | If (e1, e2, e3) ->
        let floating_binds1, e1 = aux e1 in
        let floating_binds2, e2 = aux e2 in
        let floating_binds3, e3 = aux e3 in
        ( floating_binds1 @ floating_binds2 @ floating_binds3,
          with_ (Core.If (e1, e2, e3)) )
    | InSuper e ->
        let floating_binds, e = aux e in
        (floating_binds, with_ (Core.InSuper e))
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
          with_ (Core.Object { binds = fixed_binds @ binds; assrts; fields }) )
    | ObjectFor (e1, e2, x, e3) ->
        (* FIXME: move binds outside of e1, e2, e3 *)
        let e1 = { e1 with v = local (aux e1) } in
        let e2 = { e2 with v = local (aux e2) } in
        let e3 = { e3 with v = local (aux e3) } in
        ([], with_ (Core.ObjectFor (e1, e2, x, e3)))
    | SuperIndex e ->
        let floating_binds, e = aux e in
        (floating_binds, with_ (Core.SuperIndex e))
    | Unary (op, e) ->
        let floating_binds, e = aux e in
        (floating_binds, with_ (Core.Unary (op, e)))
  in
  { e with v = local (aux e) }
