type value =
  | Null
  | True
  | False
  | String of string
  | Double of float
  | Object of object_
  | Function of
      (value Lazy.t array * (string * value Lazy.t) list -> value Lazy.t)
  | Array of value Lazy.t array

and assrts = value Lazy.t list
and fields = (string, int * value Lazy.t) Hashtbl.t

and object_ =
  | Simple of (assrts * fields)
  | General of (fields -> assrts * fields)

let empty_obj_fields = Hashtbl.create 0
let value_of_bool = function true -> True | false -> False

let get_object = function
  | Object (General obj) -> obj
  | Object (Simple obj) -> fun _ -> obj
  | _ -> failwith "expect object got something else"

let get_bool = function
  | True -> true
  | False -> false
  | _ -> failwith "expect bool got something else"

let get_array = function
  | Array xs -> xs
  | _ -> failwith "expect array got something else"

let get_double = function
  | Double f -> f
  | _ -> failwith "expect double got something else"

let get_function = function
  | Function f -> f
  | _ -> failwith "expect function got something else"

let get_string = function
  | String s -> s
  | _ -> failwith "expect string got something else"

let rec std_cmp = function
  | Array a, Array b ->
      let rec aux ia ib =
        match (ia = Array.length a, ib = Array.length b) with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false ->
            let r = std_cmp (Lazy.force a.(ia), Lazy.force b.(ib)) in
            if r = 0 then aux (ia + 1) (ib + 1) else r
      in
      aux 0 0
  | String s1, String s2 -> String.compare s1 s2
  | Double n1, Double n2 -> Float.compare n1 n2
  | _ -> failwith "std_cmp: invalid arguments"

let manifestation ppf v =
  let open Format in
  let rec aux ppf = function
    | Null -> fprintf ppf "null"
    | True -> fprintf ppf "true"
    | False -> fprintf ppf "false"
    | String s -> fprintf ppf "%S" s
    | Double f when f = (f |> int_of_float |> float_of_int) ->
        fprintf ppf "%d" (int_of_float f)
    | Double f -> fprintf ppf "%f" f
    | Array [||] -> fprintf ppf "[ ]"
    | Array xs ->
        fprintf ppf "@[<v 3>[@,%a@]@,]"
          (pp_print_array
             ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
             (fun ppf (lazy x) -> aux ppf x))
          xs
    | Function _ -> ()
    | Object _ as x ->
        let assrts, tbl = (get_object x) empty_obj_fields in
        assrts |> List.iter (fun (lazy _) -> ());
        let xs =
          tbl |> Hashtbl.to_seq |> List.of_seq
          |> List.filter_map (fun (k, (h, v)) ->
                 if h = 2 then None else Some (k, v))
          |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
        in
        if xs = [] then fprintf ppf "{ }"
        else
          fprintf ppf "@[<v 3>{@,%a@]@,}"
            (pp_print_list
               ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
               (fun ppf (k, (lazy v)) ->
                 fprintf ppf "@<0>\"@<0>%s@<0>\"@<0>:@<0> %a" k aux v))
            xs
  in
  aux ppf v;
  fprintf ppf "\n"

let std_primitive_equals ([| v; v' |], []) =
  lazy
    ((match (Lazy.force v, Lazy.force v') with
     | String lhs, String rhs -> lhs = rhs
     | Double lhs, Double rhs -> lhs = rhs
     | True, True | False, False | Null, Null -> true
     | _ -> false)
    |> value_of_bool)

let std_length ([| v |], []) =
  lazy
    (match v with
    | (lazy (Array xs)) -> Double (xs |> Array.length |> float_of_int)
    | (lazy (String s)) -> Double (s |> String.length |> float_of_int)
    | (lazy (Object _ as x)) ->
        let _, fields = (get_object x) empty_obj_fields in
        Double
          (fields |> Hashtbl.to_seq
          |> Seq.filter_map (fun (f, (h, _)) -> if h = 2 then None else Some ())
          |> Seq.length |> float_of_int)
    | _ -> failwith "std.length: invalid type argument")

let std_make_array ([| n; f |], []) =
  lazy
    (let n = n |> Lazy.force |> get_double in
     let f = f |> Lazy.force |> get_function in
     Array
       (Array.init (int_of_float n) (fun i ->
            f ([| lazy (Double (float_of_int i)) |], []))))

let std_type ([| v |], []) =
  lazy
    (match v with
    | (lazy Null) -> String "null"
    | (lazy (True | False)) -> String "boolean"
    | (lazy (String _)) -> String "string"
    | (lazy (Function _)) -> String "function"
    | (lazy (Double _)) -> String "number"
    | (lazy (Object _)) -> String "object"
    | (lazy (Array _)) -> String "array")

let std_filter ([| f; ary |], []) =
  lazy
    (let f = f |> Lazy.force |> get_function in
     let xs = ary |> Lazy.force |> get_array in
     Array
       (xs |> Array.to_list
       |> List.filter (fun x -> f ([| x |], []) |> Lazy.force |> get_bool)
       |> Array.of_list))

let std_object_has_ex ([| obj; f; b' |], []) =
  lazy
    (let _, fields = get_object (Lazy.force obj) empty_obj_fields in
     let f = f |> Lazy.force |> get_string in
     let b' = b' |> Lazy.force |> get_bool in
     match Hashtbl.find_opt fields f with
     | Some (h, _) when h <> 2 || b' -> True
     | _ -> False)

let std_object_fields_ex ([| obj; b' |], []) =
  lazy
    (let b' = b' |> Lazy.force |> get_bool in
     let _, fields = get_object (Lazy.force obj) empty_obj_fields in
     Array
       (fields |> Hashtbl.to_seq
       |> Seq.filter_map (fun (f, (h, _)) ->
              if h <> 2 || b' then Some f else None)
       |> List.of_seq |> List.sort String.compare |> Array.of_list
       |> Array.map (fun x -> lazy (String x))))

let in_super super key =
  if Hashtbl.mem super (get_string key) then True else False

let super_index super key =
  let key = get_string key in
  match Hashtbl.find_opt super key with
  | None -> failwith ("field does not exist: " ^ key)
  | Some (_, (lazy v)) -> v

let array_index f1 f2 =
  match f1 () with
  | Array a -> a.(get_double (f2 ()) |> int_of_float) |> Lazy.force
  | Object _ as x -> (
      let _, tbl = (get_object x) empty_obj_fields in
      let key = get_string (f2 ()) in
      match Hashtbl.find_opt tbl key with
      | None -> failwith ("field does not exist: " ^ key)
      | Some (_, (lazy v)) -> v)
  | _ -> failwith "ArrayIndex: expect array got something else"

let binary_add lhs rhs =
  match lhs with
  | Double f1 -> Double (f1 +. get_double rhs)
  | Array xs -> Array (Array.append xs (get_array rhs))
  | String s1 -> String (s1 ^ get_string rhs)
  | Object _ ->
      Object
        (General
           (fun super ->
             let assrts1, fields1 = (get_object lhs) super in
             let assrts2, fields2 = get_object rhs fields1 in
             let tbl = Hashtbl.create 0 in
             let common = ref [] in
             fields1
             |> Hashtbl.iter (fun f (h, v) ->
                    match Hashtbl.find_opt fields2 f with
                    | Some (h', v') -> common := (f, h, v, h', v') :: !common
                    | None -> Hashtbl.add tbl f (h, v));
             fields2
             |> Hashtbl.iter (fun f (h, v) ->
                    match Hashtbl.find_opt fields1 f with
                    | Some _ -> ()
                    | None -> Hashtbl.add tbl f (h, v));
             !common
             |> List.iter (fun (f, h1, v1, h2, v2) ->
                    let h = if h2 = 1 then h1 else h2 in
                    Hashtbl.add tbl f (h, v2));
             (assrts1 @ assrts2, tbl)))
  | _ -> failwith "invalid add"

let object_field tbl h k v =
  match k with
  | Null -> ()
  | String k -> Hashtbl.add tbl k (h, v)
  | _ -> failwith "field name must be string, got something else"

let object_field' k v =
  match k with
  | Null -> None
  | String s -> Some (s, (1, v))
  | _ -> failwith "field name must be string, got something else"

let function_param i positional id named v =
  if i < Array.length positional then positional.(i)
  else
    match List.assoc_opt id named with
    | Some x -> x
    | None -> (
        match v with Some v -> v | None -> failwith "Parameter not bound")

let if_ f1 f2 f3 =
  match f1 () with
  | True -> f2 ()
  | False -> f3 ()
  | _ -> failwith "invalid if condition"

let error v =
  manifestation Format.str_formatter v;
  failwith (Format.flush_str_formatter ())

let object_field_plus super key value tbl h =
  object_field tbl h key
    (lazy
      (if_
         (fun () -> in_super super key)
         (fun () ->
           let lhs = super_index super key in
           let rhs = value in
           binary_add lhs rhs)
         (fun () -> value)))
