type value =
  | Null
  | True
  | False
  | String of string
  | Double of float
  | Object of object_
  | Function of (value Lazy.t array * (string * value Lazy.t) list -> value)
  | Array of value Lazy.t array

and assrts = value Lazy.t list
and fields = (string, int * value Lazy.t) Hashtbl.t

and object_ =
  | General of
      ((assrts * fields (* cache *))
      * (fields (* self *) -> fields (* super *) -> assrts * fields) option)

let empty_obj_fields = Hashtbl.create 0
let gen_empty_self () = Hashtbl.create 0
let value_of_bool = function true -> True | false -> False

let string_of_double f =
  let i64 = Int64.of_float f in
  if f = Int64.to_float i64 then Printf.sprintf "%s" (Int64.to_string i64)
  else Printf.sprintf "%s" (Dtoa.ecma_string_of_float f)

let get_object = function
  | Object (General (obj, _)) -> obj
  | _ -> failwith "expect object got something else"

let get_object_f = function
  | Object (General (_, Some f)) -> f
  | Object (General (obj, None)) -> fun _ _ -> obj
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
    | String s ->
        let buf = Buffer.create (String.length s) in
        let rec loop i =
          if i >= String.length s then ()
          else (
            (match s.[i] with
            | '"' -> Buffer.add_string buf {|\"|}
            | '\\' -> Buffer.add_string buf {|\\|}
            | '\b' -> Buffer.add_string buf {|\b|}
            | '\012' -> Buffer.add_string buf {|\f|}
            | '\n' -> Buffer.add_string buf {|\n|}
            | '\r' -> Buffer.add_string buf {|\r|}
            | '\t' -> Buffer.add_string buf {|\t|}
            | '\000' -> Buffer.add_string buf {|\u0000|}
            | ch -> Buffer.add_char buf ch);
            loop (i + 1))
        in
        loop 0;
        fprintf ppf "\"%s\"" (Buffer.contents buf)
    | Double f -> fprintf ppf "%s" (string_of_double f)
    | Array [||] -> fprintf ppf "[ ]"
    | Array xs ->
        fprintf ppf "@[<v 3>[@,%a@]@,]"
          (pp_print_array
             ~pp_sep:(fun ppf () -> fprintf ppf ",@,")
             (fun ppf (lazy x) -> aux ppf x))
          xs
    | Function _ -> ()
    | Object _ as x ->
        let assrts, tbl = get_object x in
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
  (match (Lazy.force v, Lazy.force v') with
  | String lhs, String rhs -> lhs = rhs
  | Double lhs, Double rhs -> lhs = rhs
  | True, True | False, False | Null, Null -> true
  | _ -> false)
  |> value_of_bool

let std_length ([| v |], []) =
  match v with
  | (lazy (Array xs)) -> Double (xs |> Array.length |> float_of_int)
  | (lazy (String s)) -> Double (s |> String.length |> float_of_int)
  | (lazy (Object _ as x)) ->
      let _, fields = get_object x in
      Double
        (fields |> Hashtbl.to_seq
        |> Seq.filter_map (fun (f, (h, _)) -> if h = 2 then None else Some ())
        |> Seq.length |> float_of_int)
  | _ -> failwith "std.length: invalid type argument"

let std_make_array ([| n; f |], []) =
  let n = n |> Lazy.force |> get_double in
  let f = f |> Lazy.force |> get_function in
  Array
    (Array.init (int_of_float n) (fun i ->
         lazy (f ([| lazy (Double (float_of_int i)) |], []))))

let std_type' = function
  | Null -> "null"
  | True | False -> "boolean"
  | String _ -> "string"
  | Function _ -> "function"
  | Double _ -> "number"
  | Object _ -> "object"
  | Array _ -> "array"

let std_type ([| v |], []) = String (std_type' (Lazy.force v))

let std_filter ([| f; ary |], []) =
  let f = f |> Lazy.force |> get_function in
  let xs = ary |> Lazy.force |> get_array in
  Array
    (xs |> Array.to_list
    |> List.filter (fun x -> f ([| x |], []) |> get_bool)
    |> Array.of_list)

let std_object_has_ex ([| obj; f; b' |], []) =
  let _, fields = get_object (Lazy.force obj) in
  let f = f |> Lazy.force |> get_string in
  let b' = b' |> Lazy.force |> get_bool in
  match Hashtbl.find_opt fields f with
  | Some (h, _) when h <> 2 || b' -> True
  | _ -> False

let std_object_fields_ex ([| obj; b' |], []) =
  let b' = b' |> Lazy.force |> get_bool in
  let _, fields = get_object (Lazy.force obj) in
  Array
    (fields |> Hashtbl.to_seq
    |> Seq.filter_map (fun (f, (h, _)) -> if h <> 2 || b' then Some f else None)
    |> List.of_seq |> List.sort String.compare |> Array.of_list
    |> Array.map (fun x -> lazy (String x)))

let std_modulo ([| a; b |], []) =
  Double (Float.rem (get_double (Lazy.force a)) (get_double (Lazy.force b)))

let std_codepoint ([| s |], []) =
  let s = get_string (Lazy.force s) in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  match Uutf.decode d with
  | `Uchar u -> Double (float_of_int (Uchar.to_int u))
  | _ -> failwith "std.codepoint: invalid input string"

let std_char ([| n |], []) =
  let n = int_of_float (get_double (Lazy.force n)) in
  let buf = Buffer.create 10 in
  let e = Uutf.encoder `UTF_8 (`Buffer buf) in
  ignore (Uutf.encode e (`Uchar (Uchar.of_int n)));
  ignore (Uutf.encode e `End);
  String (Buffer.contents buf)

let std_floor ([| f |], []) = Double (Float.floor (get_double (Lazy.force f)))

let in_super super key =
  if Hashtbl.mem super (get_string key) then True else False

let super_index super key =
  let key = get_string key in
  match Hashtbl.find_opt super key with
  | None -> failwith ("field does not exist: " ^ key)
  | Some (_, (lazy v)) -> v

let array_index v1 v2 =
  match Lazy.force v1 with
  | Array a -> a.(get_double v2 |> int_of_float) |> Lazy.force
  | String s -> String (String.make 1 s.[int_of_float (get_double v2)])
  | Object _ as x -> (
      let assrts, tbl = get_object x in
      assrts |> List.iter (fun (lazy _) -> ());
      let key = get_string v2 in
      match Hashtbl.find_opt tbl key with
      | None -> failwith ("field does not exist: " ^ key)
      | Some (_, (lazy v)) -> v)
  | _ -> failwith "ArrayIndex: expect array got something else"

let rec value_to_string = function
  | String s -> s
  | Double f -> string_of_double f
  | True -> "true"
  | False -> "false"
  | Array xs ->
      "["
      ^ String.concat ", "
          (xs |> Array.to_list
          |> List.map (fun x -> value_to_string (Lazy.force x)))
      ^ "]"
  | v -> failwith ("value_to_string: " ^ std_type' v)

let make_object f =
  Object (General (f (gen_empty_self ()) empty_obj_fields, Some f))

let make_simple_object obj = Object (General (obj, None))

let binary_add lhs rhs =
  match (lhs, rhs) with
  | Double f1, Double f2 -> Double (f1 +. f2)
  | Array xs, _ -> Array (Array.append xs (get_array rhs))
  | String _, _ | _, String _ ->
      String (value_to_string lhs ^ value_to_string rhs)
  | Object _, _ ->
      make_object (fun self super ->
          let assrts1, _ = get_object_f lhs self super in
          let fields1 = Hashtbl.copy self in
          Hashtbl.reset self;
          let assrts2, _ = get_object_f rhs self fields1 in
          let fields2 = Hashtbl.copy self in
          Hashtbl.reset self;
          let common = ref [] in
          fields1
          |> Hashtbl.iter (fun f (h, v) ->
                 match Hashtbl.find_opt fields2 f with
                 | Some (h', v') -> common := (f, h, v, h', v') :: !common
                 | None -> Hashtbl.add self f (h, v));
          fields2
          |> Hashtbl.iter (fun f (h, v) ->
                 match Hashtbl.find_opt fields1 f with
                 | Some _ -> ()
                 | None -> Hashtbl.add self f (h, v));
          !common
          |> List.iter (fun (f, h1, v1, h2, v2) ->
                 let h = if h2 = 1 then h1 else h2 in
                 Hashtbl.add self f (h, v2));
          (assrts1 @ assrts2, self))
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

let if_ v1 f2 f3 =
  match v1 with
  | True -> f2 ()
  | False -> f3 ()
  | _ -> failwith "invalid if condition"

let error v =
  manifestation Format.str_formatter v;
  failwith (Format.flush_str_formatter ())

let object_field_plus super key value tbl h =
  object_field tbl h key
    (lazy
      (if_ (in_super super key)
         (fun () ->
           let lhs = super_index super key in
           let rhs = value in
           binary_add lhs rhs)
         (fun () -> value)))

let append_to_std tbl =
  Hashtbl.add tbl "primitiveEquals" (1, lazy (Function std_primitive_equals));
  Hashtbl.add tbl "length" (1, lazy (Function std_length));
  Hashtbl.add tbl "makeArray" (1, lazy (Function std_make_array));
  Hashtbl.add tbl "type" (1, lazy (Function std_type));
  Hashtbl.add tbl "filter" (1, lazy (Function std_filter));
  Hashtbl.add tbl "objectHasEx" (1, lazy (Function std_object_has_ex));
  Hashtbl.add tbl "objectFieldsEx" (1, lazy (Function std_object_fields_ex));
  Hashtbl.add tbl "modulo" (1, lazy (Function std_modulo));
  Hashtbl.add tbl "codepoint" (1, lazy (Function std_codepoint));
  Hashtbl.add tbl "char" (1, lazy (Function std_char));
  Hashtbl.add tbl "floor" (1, lazy (Function std_floor));
  ()
