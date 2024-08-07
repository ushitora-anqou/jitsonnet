open struct
  type variable = Self | Super | Var of string [@@deriving show]

  module G = Set.Make (struct
    type t = variable

    let compare = compare
  end)

  type t = { g : G.t }

  let is_in g x =
    if G.mem x g.g then Ok ()
    else Error (Printf.sprintf "variable %s not found" (show_variable x))

  let for_all f xs =
    let rec aux = function
      | [] -> Ok ()
      | x :: xs -> ( match f x with Ok _ -> aux xs | e -> e)
    in
    aux xs

  let add x g = Ok { g = G.add x g.g }

  let should_be_unique where xs =
    if List.length (List.sort_uniq compare xs) = List.length xs then Ok ()
    else Error ("Statich_check: duplicated: " ^ where)
end

let rec f g (n : Syntax.Core.expr) =
  let open Syntax.Core in
  let ( let* ) = Result.bind in
  match n.v with
  | Null | True | False | String _ | Number _ | Import _ | Importstr _
  | Importbin _ ->
      Ok ()
  | Self -> is_in g Self
  | InSuper e | SuperIndex e ->
      let* () = is_in g Super in
      let* _ = f g e in
      Ok ()
  | Object { binds; assrts; fields } ->
      let* _ = fields |> List.map (fun (e, _, _, _) -> e) |> for_all (f g) in
      let* g' =
        binds
        |> List.fold_left
             (fun g (x, _) -> Result.bind g (fun g -> add (Var x) g))
             (Ok g)
      in
      let* _ =
        fields
        |> List.map (fun (_, _, _, e') -> e')
        |> for_all (fun e ->
               let* g' = g' |> add Self in
               let* g' = g' |> add Super in
               f g' e)
      in
      let* _ =
        assrts
        |> for_all (fun e ->
               let* g' = g' |> add Self in
               let* g' = g' |> add Super in
               f g' e)
      in
      let* _ =
        fields
        |> List.filter_map (function
             | { v = String s; _ }, _, _, _ -> Some s
             | _ -> None)
        |> should_be_unique "object"
      in
      Ok ()
  | ObjectFor (e1, e2, x, e3) ->
      let* _ =
        let* g' = g |> add (Var x) in
        e1 |> f g'
      in
      let* _ =
        let* g = g |> add (Var x) in
        let* g = g |> add Self in
        let* g = g |> add Super in
        e2 |> f g
      in
      let* _ = e3 |> f g in
      Ok ()
  | Array es -> es |> for_all (f g)
  | ArrayIndex (e, e') ->
      let* _ = f g e in
      let* _ = f g e' in
      Ok ()
  | Call (e, xs, ys) ->
      let* _ = f g e in
      let* _ = xs |> for_all (f g) in
      let* _ = ys |> List.map snd |> for_all (f g) in
      let* _ = ys |> List.map fst |> should_be_unique "call" in
      Ok ()
  | Var x -> is_in g (Var x)
  | Local (xs, e) ->
      let* g' =
        xs
        |> List.fold_left
             (fun g (x, _) -> Result.bind g (fun g -> add (Var x) g))
             (Ok g)
      in
      let* _ = xs |> List.map snd |> for_all (f g') in
      let* _ = f g' e in
      let* _ = xs |> List.map fst |> should_be_unique "local" in
      Ok ()
  | If (e1, e2, e3) ->
      let* _ = f g e1 in
      let* _ = f g e2 in
      let* _ = f g e3 in
      Ok ()
  | Binary (el, _, er) ->
      let* _ = f g el in
      let* _ = f g er in
      Ok ()
  | Unary (_, e) ->
      let* _ = f g e in
      Ok ()
  | Function (xs, e') ->
      let* g' =
        xs
        |> List.fold_left
             (fun g (x, _) -> Result.bind g (fun g -> add (Var x) g))
             (Ok g)
      in
      let* _ = xs |> List.filter_map snd |> for_all (f g') in
      let* _ = f g' e' in
      let* _ = xs |> List.map fst |> should_be_unique "function" in
      Ok ()
  | Error e -> f g e

let f is_stdjsonnet =
  f { g = (if is_stdjsonnet then G.empty else G.add (Var "$std") G.empty) }
