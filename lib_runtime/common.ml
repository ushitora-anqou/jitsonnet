module SmartString : sig
  type t

  val length : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val get : t -> int -> char
  val sub : t -> int -> int -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val concat2 : t -> t -> t
  val concat : t -> t list -> t
end = struct
  type t = string

  let make v = v
  let length x = match x with s -> String.length s
  let of_string x = x
  let to_string x = match x with s -> s
  let get x i = match x with s -> String.get s i
  let sub x off len = match x with s -> make (String.sub s off len)
  let compare x y = match (x, y) with x, y -> String.compare x y
  let equal x y = match (x, y) with x, y -> String.equal x y

  let concat2 x y =
    let rec aux = function x, y -> x ^ y in
    make (aux (x, y))

  let concat = String.concat
end

type value =
  | Null
  | True
  | False
  | SmartString of SmartString.t
  | Double of float
  | Object of object_
  | Function of
      int (* # of args *)
      * (value Lazy.t array * (string * value Lazy.t) list -> value)
  | Array of value Lazy.t array

and value_ary = value Lazy.t array
and assrts = value Lazy.t list
and fields = (string, int * value Lazy.t) Hashtbl.t

and object_ =
  | General of
      ((value_ary * assrts * fields (* cache *))
      * (fields (* self *) -> fields (* super *) -> value_ary * assrts * fields)
        option)

let empty_obj_fields = Hashtbl.create 0
let gen_empty_self () = Hashtbl.create 0
let value_of_bool = function true -> True | false -> False

let string_of_double f =
  if Float.floor f = f then Printf.sprintf "%.0f" f
  else Dtoa.ecma_string_of_float f

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
  | Function (_, f) -> f
  | _ -> failwith "expect function got something else"

let get_string = function
  | SmartString s -> SmartString.to_string s
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
  | SmartString s1, SmartString s2 -> SmartString.compare s1 s2
  | Double n1, Double n2 -> Float.compare n1 n2
  | _ -> failwith "std_cmp: invalid arguments"

let function_param i positional id named v =
  if i < Array.length positional then positional.(i)
  else
    match List.assoc_opt id named with
    | Some x -> x
    | None -> (
        match v with Some v -> v | None -> failwith "Parameter not bound")

let eval_asserts assrts = List.iter (fun (lazy _) -> ()) assrts

let extract_visible_fields tbl =
  tbl |> Hashtbl.to_seq |> List.of_seq
  |> List.filter_map (fun (k, (h, v)) -> if h = 2 then None else Some (k, v))
  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)

let quoted_string s =
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
  "\"" ^ Buffer.contents buf ^ "\""

let manifestation ?(multi_line = true) ppf v =
  let open Format in
  let rec aux ppf = function
    | Null -> fprintf ppf "null"
    | True -> fprintf ppf "true"
    | False -> fprintf ppf "false"
    | SmartString s ->
        fprintf ppf "%s" (quoted_string (SmartString.to_string s))
    | Double f -> fprintf ppf "%s" (string_of_double f)
    | Array [||] -> fprintf ppf "[ ]"
    | Array xs ->
        fprintf ppf
          ("@[<v 3>["
          ^^ (if multi_line then "@," else "")
          ^^ "%a@]"
          ^^ (if multi_line then "@," else "")
          ^^ "]")
          (pp_print_array
             ~pp_sep:(fun ppf () ->
               fprintf ppf ("," ^^ if multi_line then "@," else " "))
             (fun ppf (lazy x) -> aux ppf x))
          xs
    | Function (_, f) -> aux ppf (f ([||], []))
    | Object _ as x -> (
        let _, assrts, tbl = get_object x in
        eval_asserts assrts;
        match extract_visible_fields tbl with
        | [] -> fprintf ppf "{ }"
        | xs ->
            fprintf ppf
              ("@[<v 3>{"
              ^^ (if multi_line then "@," else "")
              ^^ "%a@]"
              ^^ (if multi_line then "@," else "")
              ^^ "}")
              (pp_print_list
                 ~pp_sep:(fun ppf () ->
                   fprintf ppf ("," ^^ if multi_line then "@," else " "))
                 (fun ppf (k, (lazy v)) ->
                   fprintf ppf "@<0>%s@<0>:@<0> %a" (quoted_string k) aux v))
              xs)
  in
  aux ppf v;
  if multi_line then fprintf ppf "\n";
  fprintf ppf "@?"

let string_manifestation = function
  | SmartString s -> print_string (SmartString.to_string s)
  | _ -> failwith "expect string, but got something else"

let multi_manifestation ~target_dir ~string v =
  let rec mkpath = function
    | "" -> ()
    | path when Sys.file_exists path && Sys.is_directory path -> ()
    | path when Sys.file_exists path ->
        failwith ("mkpath: already file exists: " ^ path)
    | path ->
        mkpath (Filename.dirname path);
        Sys.mkdir path 0o775
  in
  let with_file k f =
    let file_path = Filename.concat target_dir k in
    mkpath (Filename.dirname file_path);
    let oc = open_out_bin file_path in
    Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)
  in
  match Lazy.force v with
  | Object (General ((_, assrts, tbl), _)) ->
      eval_asserts assrts;
      tbl |> extract_visible_fields
      |> List.iter (function
           | k, (lazy (SmartString s)) when string ->
               with_file k (fun oc ->
                   Out_channel.output_string oc (SmartString.to_string s))
           | _ when string ->
               failwith "expect string values in object, got something else"
           | k, (lazy v) ->
               with_file k (fun oc ->
                   manifestation (Format.formatter_of_out_channel oc) v))
  | _ -> failwith "expect object, got something else"

let std_primitive_equals ([| v; v' |], []) =
  (match (Lazy.force v, Lazy.force v') with
  | SmartString lhs, SmartString rhs -> SmartString.equal lhs rhs
  | Double lhs, Double rhs -> lhs = rhs
  | True, True | False, False | Null, Null -> true
  | _ -> false)
  |> value_of_bool

let std_length (positional, named) =
  let v = function_param 0 positional "x" named None in
  match v with
  | (lazy (Array xs)) -> Double (xs |> Array.length |> float_of_int)
  | (lazy (SmartString s)) -> Double (float_of_int (SmartString.length s))
  | (lazy (Object _ as x)) ->
      let _, _, fields = get_object x in
      Double
        (fields |> Hashtbl.to_seq
        |> Seq.filter_map (fun (f, (h, _)) -> if h = 2 then None else Some ())
        |> Seq.length |> float_of_int)
  | (lazy (Function (n, _))) -> Double (float_of_int n)
  | _ -> failwith "std.length: invalid type argument"

let std_type' = function
  | Null -> "null"
  | True | False -> "boolean"
  | SmartString _ -> "string"
  | Function _ -> "function"
  | Double _ -> "number"
  | Object _ -> "object"
  | Array _ -> "array"

let std_type (positional, named) =
  let v = function_param 0 positional "x" named None in
  SmartString (SmartString.of_string (std_type' (Lazy.force v)))

let std_filter (positional, named) =
  let f = function_param 0 positional "func" named None in
  let ary = function_param 1 positional "arr" named None in
  let f = f |> Lazy.force |> get_function in
  let xs = ary |> Lazy.force |> get_array in
  Array
    (xs |> Array.to_list
    |> List.filter (fun x -> f ([| x |], []) |> get_bool)
    |> Array.of_list)

let std_object_has_ex ([| obj; f; b' |], []) =
  let _, _, fields = get_object (Lazy.force obj) in
  let f = f |> Lazy.force |> get_string in
  let b' = b' |> Lazy.force |> get_bool in
  match Hashtbl.find_opt fields f with
  | Some (h, _) when h <> 2 || b' -> True
  | _ -> False

let std_object_fields_ex ([| obj; b' |], []) =
  let b' = b' |> Lazy.force |> get_bool in
  let _, _, fields = get_object (Lazy.force obj) in
  Array
    (fields |> Hashtbl.to_seq
    |> Seq.filter_map (fun (f, (h, _)) -> if h <> 2 || b' then Some f else None)
    |> List.of_seq |> List.sort String.compare |> Array.of_list
    |> Array.map (fun x -> lazy (SmartString (SmartString.of_string x))))

let std_modulo ([| a; b |], []) =
  Double (Float.rem (get_double (Lazy.force a)) (get_double (Lazy.force b)))

let std_codepoint (positional, named) =
  let s = function_param 0 positional "str" named None in
  let s = get_string (Lazy.force s) in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  match Uutf.decode d with
  | `Uchar u -> Double (float_of_int (Uchar.to_int u))
  | _ -> failwith "std.codepoint: invalid input string"

let std_char (positional, named) =
  let n = function_param 0 positional "n" named None in
  let n = int_of_float (get_double (Lazy.force n)) in
  let buf = Buffer.create 10 in
  let e = Uutf.encoder `UTF_8 (`Buffer buf) in
  ignore (Uutf.encode e (`Uchar (Uchar.of_int n)));
  ignore (Uutf.encode e `End);
  SmartString (SmartString.of_string (Buffer.contents buf))

let std_floor (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.floor (get_double (Lazy.force f)))

let std_acos (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.acos (get_double (Lazy.force f)))

let std_asin (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.asin (get_double (Lazy.force f)))

let std_atan (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.atan (get_double (Lazy.force f)))

let std_cos (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.cos (get_double (Lazy.force f)))

let std_sin (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.sin (get_double (Lazy.force f)))

let std_tan (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.tan (get_double (Lazy.force f)))

let std_exp (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.exp (get_double (Lazy.force f)))

let std_log (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.log (get_double (Lazy.force f)))

let std_sqrt (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.sqrt (get_double (Lazy.force f)))

let std_exponent (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (float_of_int (snd (Float.frexp (get_double (Lazy.force f)))))

let std_mantissa (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (fst (Float.frexp (get_double (Lazy.force f))))

let std_pow (positional, named) =
  let f1 = function_param 0 positional "x" named None in
  let f2 = function_param 1 positional "n" named None in
  Double (Float.pow (get_double (Lazy.force f1)) (get_double (Lazy.force f2)))

let std_ceil (positional, named) =
  let f = function_param 0 positional "x" named None in
  Double (Float.ceil (get_double (Lazy.force f)))

let std_md5 (positional, named) =
  let s = function_param 0 positional "s" named None in
  SmartString
    (SmartString.of_string
       (Digest.to_hex (Digest.string (get_string (Lazy.force s)))))

let std_sha1 (positional, named) =
  let s = function_param 0 positional "s" named None in
  SmartString
    (SmartString.of_string
       (Digestif.SHA1.to_hex
          (Digestif.SHA1.digest_string (get_string (Lazy.force s)))))

let std_sha256 (positional, named) =
  let s = function_param 0 positional "s" named None in
  SmartString
    (SmartString.of_string
       (Digestif.SHA256.to_hex
          (Digestif.SHA256.digest_string (get_string (Lazy.force s)))))

let std_sha512 (positional, named) =
  let s = function_param 0 positional "s" named None in
  SmartString
    (SmartString.of_string
       (Digestif.SHA512.to_hex
          (Digestif.SHA512.digest_string (get_string (Lazy.force s)))))

let std_sha3 (positional, named) =
  let s = function_param 0 positional "s" named None in
  SmartString
    (SmartString.of_string
       (Digestif.SHA3_512.to_hex
          (Digestif.SHA3_512.digest_string (get_string (Lazy.force s)))))

let std_decode_utf8 (positional, named) =
  let arr = function_param 0 positional "arr" named None in
  let arr = get_array (Lazy.force arr) in
  SmartString
    (SmartString.of_string
       (String.init (Array.length arr) (fun i ->
            char_of_int (int_of_float (get_double (Lazy.force arr.(i)))))))

let std_encode_utf8 (positional, named) =
  let str = function_param 0 positional "str" named None in
  let str = get_string (Lazy.force str) in
  Array
    (Array.init (String.length str) (fun i ->
         Lazy.from_val (Double (float_of_int (int_of_char str.[i])))))

let in_super super key = value_of_bool (Hashtbl.mem super (get_string key))

let super_index super key =
  let key = get_string key in
  match Hashtbl.find_opt super key with
  | None -> failwith ("field does not exist: " ^ key)
  | Some (_, (lazy v)) -> v

let array_index_s' (_, assrts, tbl) key =
  eval_asserts assrts;
  match Hashtbl.find_opt tbl key with
  | None -> failwith ("field does not exist: " ^ key)
  | Some (_, (lazy v)) -> v

let array_index_s v1 key =
  match Lazy.force v1 with
  | Object (General (obj, _)) -> array_index_s' obj key
  | _ -> failwith "ArrayIndex: expect array got something else"

let array_index v1 v2 =
  match v1 with
  | Array a -> a.(get_double v2 |> int_of_float) |> Lazy.force
  | SmartString s ->
      SmartString (SmartString.sub s (int_of_float (get_double v2)) 1)
  | Object (General (obj, _)) ->
      let key = get_string v2 in
      array_index_s' obj key
  | _ -> failwith "ArrayIndex: expect array got something else"

let rec value_to_smart_string = function
  | SmartString s -> s
  | Double f -> SmartString.of_string (string_of_double f)
  | True -> SmartString.of_string "true"
  | False -> SmartString.of_string "false"
  | (Array _ | Object _) as x ->
      manifestation ~multi_line:false Format.str_formatter x;
      SmartString.of_string (Format.flush_str_formatter ())
  | v -> failwith ("value_to_smart_string: " ^ std_type' v)

let make_object f =
  Object (General (f (gen_empty_self ()) empty_obj_fields, Some f))

let make_simple_object obj = Object (General (obj, None))

let binary_add lhs rhs =
  match (lhs, rhs) with
  | Double f1, Double f2 -> Double (f1 +. f2)
  | Array xs, _ -> Array (Array.append xs (get_array rhs))
  | SmartString _, _ | _, SmartString _ ->
      SmartString
        (SmartString.concat2
           (value_to_smart_string lhs)
           (value_to_smart_string rhs))
  | Object _, _ ->
      make_object (fun self super ->
          let _, assrts1, _ = get_object_f lhs self super in
          let fields1 = Hashtbl.copy self in
          Hashtbl.reset self;
          let _, assrts2, _ = get_object_f rhs self fields1 in
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
          ([||], assrts1 @ assrts2, self))
  | _ -> failwith "invalid add"

let object_field tbl h k v =
  match k with
  | Null -> ()
  | SmartString k -> Hashtbl.add tbl (SmartString.to_string k) (h, v)
  | _ -> failwith "field name must be string, got something else"

let object_field' k v =
  match k with
  | Null -> None
  | SmartString s -> Some (SmartString.to_string s, (1, v))
  | _ -> failwith "field name must be string, got something else"

let error v =
  manifestation Format.str_formatter v;
  failwith (Format.flush_str_formatter ())

let object_field_plus_value super key value =
  lazy
    (if get_bool (in_super super key) then
       let lhs = super_index super key in
       let rhs = value in
       binary_add lhs rhs
     else value)

let object_field_plus super key value tbl h =
  object_field tbl h key (object_field_plus_value super key value)

let std_make_array (positional, named) =
  let n =
    get_double (Lazy.force (function_param 0 positional "sz" named None))
  in
  let f =
    get_function (Lazy.force (function_param 1 positional "func" named None))
  in
  Array
    (Array.init (int_of_float n) (fun i ->
         lazy (f ([| lazy (Double (float_of_int i)) |], []))))

let make_std_ext_var tbl
    ([| (name : value Lazy.t) |], ([] : (string * value Lazy.t) list)) =
  match Hashtbl.find_opt tbl (get_string (Lazy.force name)) with
  | None -> failwith "std.extVar: not found"
  | Some v -> Lazy.force v

let std_equals (positional, named) =
  let rec aux a b =
    match (Lazy.force a, Lazy.force b) with
    | SmartString s1, SmartString s2 -> SmartString.equal s1 s2
    | Double n1, Double n2 -> Float.equal n1 n2
    | Null, Null | True, True | False, False -> true
    | Array a1, Array a2 when Array.length a1 = Array.length a2 ->
        Array.for_all2 aux a1 a2
    | ( Object (General ((_, _, fields1), _)),
        Object (General ((_, _, fields2), _)) ) ->
        let aux' fields2 k1 v1 res =
          res
          &&
          match v1 with
          | 2, _ -> res
          | _, v1 -> (
              match Hashtbl.find_opt fields2 k1 with
              | None | Some (2, _) -> false
              | Some (_, v2) -> aux v1 v2)
        in
        Hashtbl.fold (aux' fields2) fields1 true
        && Hashtbl.fold (aux' fields1) fields2 true
    | _ -> false
  in
  let a = function_param 0 positional "a" named None in
  let b = function_param 1 positional "b" named None in
  value_of_bool (aux a b)

let std_parse_json (positional, named) =
  let str = function_param 0 positional "str" named None in
  let str = get_string (Lazy.force str) in
  let rec aux : Yojson.Safe.t -> value = function
    | `Null -> Null
    | `Bool true -> True
    | `Bool false -> False
    | `Int i -> Double (float_of_int i)
    | `Intlit s -> Double (float_of_string s)
    | `Float f -> Double f
    | `String s -> SmartString (SmartString.of_string s)
    | `Assoc xs ->
        let fields = Hashtbl.create (List.length xs) in
        List.iter
          (fun (k, v) -> Hashtbl.add fields k (1, Lazy.from_val (aux v)))
          xs;
        make_simple_object ([||], [], fields)
    | `List xs ->
        Array (Array.of_list (List.map (fun x -> Lazy.from_val (aux x)) xs))
    | _ -> assert false
  in
  aux (Yojson.Safe.from_string str)

let std_parse_yaml =
  let separator = Str.regexp "^---[ \t]*$" in
  fun (positional, named) ->
    let str = function_param 0 positional "str" named None in
    let str = get_string (Lazy.force str) in
    let rec aux : Yaml.value -> value = function
      | `Null -> Null
      | `Bool true -> True
      | `Bool false -> False
      | `Float f -> Double f
      | `String s -> SmartString (SmartString.of_string s)
      | `A xs ->
          Array (Array.of_list (List.map (fun x -> Lazy.from_val (aux x)) xs))
      | `O xs ->
          let fields = Hashtbl.create (List.length xs) in
          List.iter
            (fun (k, v) -> Hashtbl.add fields k (1, Lazy.from_val (aux v)))
            xs;
          make_simple_object ([||], [], fields)
    in
    match
      Str.split separator str |> List.map Yaml.of_string
      |> List.fold_left
           (fun acc x ->
             match (acc, x) with
             | Some acc, Ok x -> Some (aux x :: acc)
             | _ -> None)
           (Some [])
    with
    | Some [ x ] -> x
    | Some xs ->
        Array (xs |> List.map Lazy.from_val |> List.rev |> Array.of_list)
    | None -> failwith "std.parseYaml: failed to parse YAML"

let append_to_std tbl =
  Hashtbl.add tbl "primitiveEquals"
    (1, lazy (Function (2, std_primitive_equals)));
  Hashtbl.add tbl "length" (1, lazy (Function (1, std_length)));
  Hashtbl.add tbl "makeArray" (1, lazy (Function (2, std_make_array)));
  Hashtbl.add tbl "type" (1, lazy (Function (1, std_type)));
  Hashtbl.add tbl "filter" (1, lazy (Function (2, std_filter)));
  Hashtbl.add tbl "objectHasEx" (1, lazy (Function (3, std_object_has_ex)));
  Hashtbl.add tbl "objectFieldsEx" (1, lazy (Function (2, std_object_fields_ex)));
  Hashtbl.add tbl "modulo" (1, lazy (Function (2, std_modulo)));
  Hashtbl.add tbl "codepoint" (1, lazy (Function (1, std_codepoint)));
  Hashtbl.add tbl "char" (1, lazy (Function (1, std_char)));
  Hashtbl.add tbl "floor" (1, lazy (Function (1, std_floor)));
  Hashtbl.add tbl "acos" (1, lazy (Function (1, std_acos)));
  Hashtbl.add tbl "asin" (1, lazy (Function (1, std_asin)));
  Hashtbl.add tbl "atan" (1, lazy (Function (1, std_atan)));
  Hashtbl.add tbl "cos" (1, lazy (Function (1, std_cos)));
  Hashtbl.add tbl "sin" (1, lazy (Function (1, std_sin)));
  Hashtbl.add tbl "tan" (1, lazy (Function (1, std_tan)));
  Hashtbl.add tbl "exp" (1, lazy (Function (2, std_exp)));
  Hashtbl.add tbl "log" (1, lazy (Function (1, std_log)));
  Hashtbl.add tbl "sqrt" (1, lazy (Function (1, std_sqrt)));
  Hashtbl.add tbl "pow" (1, lazy (Function (2, std_pow)));
  Hashtbl.add tbl "ceil" (1, lazy (Function (1, std_ceil)));
  Hashtbl.add tbl "exponent" (1, lazy (Function (1, std_exponent)));
  Hashtbl.add tbl "mantissa" (1, lazy (Function (1, std_mantissa)));
  Hashtbl.add tbl "md5" (1, lazy (Function (1, std_md5)));
  Hashtbl.add tbl "sha1" (1, lazy (Function (1, std_sha1)));
  Hashtbl.add tbl "sha256" (1, lazy (Function (1, std_sha256)));
  Hashtbl.add tbl "sha512" (1, lazy (Function (1, std_sha512)));
  Hashtbl.add tbl "sha3" (1, lazy (Function (1, std_sha3)));
  Hashtbl.add tbl "equals" (1, lazy (Function (2, std_equals)));
  Hashtbl.add tbl "decodeUTF8" (1, lazy (Function (1, std_decode_utf8)));
  Hashtbl.add tbl "encodeUTF8" (1, lazy (Function (1, std_encode_utf8)));
  Hashtbl.add tbl "parseJson" (1, lazy (Function (1, std_parse_json)));
  Hashtbl.add tbl "parseYaml" (1, lazy (Function (1, std_parse_yaml)));
  ()
