open struct
  type variable = Self | Super | Var of string [@@deriving show]

  module G = Set.Make (struct
    type t = variable

    let compare = compare
  end)

  let is_in g x =
    if G.mem x g then Ok ()
    else Error (Printf.sprintf "variable %s not found" (show_variable x))

  let for_all f xs =
    let rec aux = function
      | [] -> Ok ()
      | x :: xs -> ( match f x with Ok _ -> aux xs | e -> e)
    in
    aux xs

  let add = G.add

  let should_be_unique xs =
    if List.length (List.sort_uniq compare xs) = List.length xs then Ok ()
    else Error "duplicated"
end

let rec f g =
  let open Syntax.Core in
  let ( let* ) = Result.bind in
  function
  | Null | True | False | String _ | Number _ -> Ok ()
  | Self -> is_in g Self
  | Super -> is_in g Super
  | Object (assrts, fields) ->
      let* _ = fields |> List.map (fun (e, _, _) -> e) |> for_all (f g) in
      let* _ =
        fields
        |> List.map (fun (_, _, e') -> e')
        |> for_all (f (g |> add Self |> add Super))
      in
      let* _ = assrts |> for_all (f (g |> add Self |> add Super)) in
      let* _ =
        fields
        |> List.filter_map (function String s, _, _ -> Some s | _ -> None)
        |> should_be_unique
      in
      Ok ()
  | ObjectFor (e1, e2, x, e3) ->
      let* _ = e1 |> f (g |> add (Var x)) in
      let* _ = e2 |> f (g |> add (Var x) |> add Self |> add Super) in
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
      let* _ = ys |> List.map fst |> should_be_unique in
      Ok ()
  | Var x -> is_in g (Var x)
  | Local (xs, e) ->
      let g' = xs |> List.fold_left (fun g (x, _) -> add (Var x) g) g in
      let* _ = xs |> List.map snd |> for_all (f g') in
      let* _ = f g' e in
      let* _ = xs |> List.map fst |> should_be_unique in
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
      let g' = xs |> List.fold_left (fun g (x, _) -> add (Var x) g) g in
      let* _ = xs |> List.map snd |> for_all (f g') in
      let* _ = f g' e' in
      let* _ = xs |> List.map fst |> should_be_unique in
      Ok ()
  | Error e -> f g e

let f = f (G.empty |> add (Var "std"))
