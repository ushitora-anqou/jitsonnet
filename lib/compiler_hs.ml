module Haskell = struct
  type expr =
    | Symbol of string
    | Call of (expr * expr)
    | StringLiteral of string
    | FloatLiteral of float
    | IntLiteral of int
    | List of expr list
    | Function of (string * expr)
    | Let of ((string * expr) list * expr)
    | Tuple of expr list
    | Do of expr list
    | Assign of (string * expr)
    | RecordConstruction of (string * (string * expr) list)
    | BoolLiteral of bool

  let pp_expr ppf e =
    let rec aux ppf = function
      | BoolLiteral true -> Format.fprintf ppf "True"
      | BoolLiteral false -> Format.fprintf ppf "False"
      | Symbol s -> Format.fprintf ppf "%s" s
      | Call (e1, e2) -> Format.fprintf ppf "(%a %a)" aux e1 aux e2
      | StringLiteral src ->
          let dec = Uutf.decoder ~encoding:`UTF_8 (`String src) in
          let rec loop off =
            match Uutf.decode dec with
            | `Uchar c ->
                let off' = Uutf.decoder_byte_count dec in
                Format.pp_print_string ppf
                  (match Uchar.to_int c with
                  | 0x00 -> {|\0|}
                  | 0x07 -> {|\a|}
                  | 0x08 -> {|\b|}
                  | 0x09 -> {|\t|}
                  | 0x0a -> {|\n|}
                  | 0x0b -> {|\v|}
                  | 0x0c -> {|\f|}
                  | 0x0d -> {|\r|}
                  | 0x22 -> {|\"|}
                  | 0x5c -> {|\\|}
                  | _ when Uucp.Gc.general_category c = `Cc ->
                      {|\|} ^ string_of_int (Uchar.to_int c)
                  | _ -> String.sub src off (off' - off));
                loop off'
            | `End -> ()
            | _ -> assert false
          in
          Format.pp_print_char ppf '"';
          loop 0;
          Format.pp_print_char ppf '"'
      | IntLiteral i -> Format.fprintf ppf "%d" i
      | FloatLiteral f -> Format.fprintf ppf "%f" f
      | List xs ->
          Format.fprintf ppf "[%a]"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
               aux)
            xs
      | Function (param, body) ->
          Format.fprintf ppf "(\\%s -> %a)" param aux body
      | Let (binds, body) ->
          Format.fprintf ppf "(let %a in %a)"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf " ; ")
               (fun ppf (id, e) -> Format.fprintf ppf "%s = %a" id aux e))
            binds aux body
      | Tuple xs ->
          Format.fprintf ppf "(%a)"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
               aux)
            xs
      | Do xs ->
          Format.fprintf ppf "(do %a)"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf " ; ")
               (fun ppf e -> Format.fprintf ppf "%a" aux e))
            xs
      | Assign (id, e) -> Format.fprintf ppf "%s <- %a" id aux e
      | RecordConstruction (id, args) ->
          Format.fprintf ppf "(%s { %a })" id
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf " , ")
               (fun ppf (id, e) -> Format.fprintf ppf "%s = %a" id aux e))
            args
    in
    aux ppf e

  let show_expr e =
    pp_expr Format.str_formatter e;
    Format.flush_str_formatter ()
end

let get_import_id kind file_path =
  (match kind with
  | `Import -> "prog/"
  | `Importbin -> "bin/"
  | `Importstr -> "str/")
  ^ Unix.realpath file_path

let callstack_varname = "cs"

type var_desc =
  | VarUnknown
  | VarObjectLocal of
      (string (* var name of self in generated code *)
      * string (* var name of super in generated code *))

type var_in_env = { name_in_target : string; desc : var_desc }
type env = { vars : (string, var_in_env) Hashtbl.t; is_stdjsonnet : bool }

let varname env name = (Hashtbl.find env.vars name).name_in_target

let set_var_desc env name desc =
  let var = Hashtbl.find env.vars name in
  let var = { var with desc } in
  Hashtbl.replace env.vars name var

let gensym =
  let i = ref 0 in
  fun suffix ->
    i := !i + 1;
    "var_" ^ string_of_int !i ^ "_"
    ^
    let suffix =
      if String.length suffix < 10 then suffix else String.sub suffix 0 10
    in
    let suffix = suffix |> Str.(global_replace (regexp {|[^a-zA-Z0-9]|}) "_") in
    suffix

let with_binds env ids f =
  ids
  |> List.iter (fun id ->
         Hashtbl.add env.vars id
           { name_in_target = gensym id; desc = VarUnknown });
  Fun.protect ~finally:(fun () -> ids |> List.iter (Hashtbl.remove env.vars)) f

let make_call func args =
  let open Haskell in
  args |> List.fold_left (fun acc x -> Call (acc, x)) func

let make_let binds body =
  match binds with [] -> body | _ -> Haskell.Let (binds, body)

let compile_builtin_std = function
  | "length" -> Ok "stdLength"
  | "makeArray" -> Ok "stdMakeArray"
  | "type" -> Ok "stdType"
  | "primitiveEquals" -> Ok "stdPrimitiveEquals"
  | "filter" -> Ok "stdFilter"
  | "objectHasEx" -> Ok "stdObjectHasEx"
  | "objectFieldsEx" -> Ok "stdObjectFieldsEx"
  | "modulo" -> Ok "stdModulo"
  | "codepoint" -> Ok "stdCodepoint"
  | "char" -> Ok "stdChar"
  | "floor" -> Ok "stdFloor"
  | "acos" -> Ok "stdAcos"
  | "asin" -> Ok "stdAsin"
  | "atan" -> Ok "stdAtan"
  | "pow" -> Ok "stdPow"
  | "ceil" -> Ok "stdCeil"
  | "cos" -> Ok "stdCos"
  | "sin" -> Ok "stdSin"
  | "tan" -> Ok "stdTan"
  | "exp" -> Ok "stdExp"
  | "log" -> Ok "stdLog"
  | "sqrt" -> Ok "stdSqrt"
  | "exponent" -> Ok "stdExponent"
  | "mantissa" -> Ok "stdMantissa"
  | "md5" -> Ok "stdMd5"
  | _ -> Error "not found"

let compile_loc = function
  | None -> Haskell.Symbol "Nothing"
  | Some loc ->
      make_call (Symbol "Just")
        [
          Tuple
            [
              StringLiteral loc.Syntax.startpos.fname;
              IntLiteral loc.Syntax.startpos.line;
              IntLiteral loc.Syntax.startpos.column;
              IntLiteral loc.Syntax.endpos.line;
              IntLiteral loc.Syntax.endpos.column;
            ];
        ]

let compile_with_loc env loc e =
  if env.is_stdjsonnet then e
  else
    make_call (Symbol "withLoc")
      [
        Symbol callstack_varname;
        compile_loc loc;
        Function (callstack_varname, e);
      ]

let compile_with_name env name e =
  if env.is_stdjsonnet then e
  else
    make_call (Symbol "withName")
      [
        Symbol callstack_varname;
        StringLiteral name;
        Function (callstack_varname, e);
      ]

let rec compile_expr ?toplevel:_ env (e0 : Syntax.Core.expr) : Haskell.expr =
  match e0.v with
  | Null -> Symbol "Null"
  | True -> make_call (Symbol "Bool") [ Symbol "True" ]
  | False -> make_call (Symbol "Bool") [ Symbol "False" ]
  | String s -> Call (Symbol "makeString", StringLiteral s)
  | Number n -> Call (Symbol "Number", FloatLiteral n)
  | Array xs ->
      Call (Symbol "makeArrayFromList", List (List.map (compile_expr env) xs))
  | ArrayIndex (e1, e2) -> compile_binary env "arrayIndex" e1 e2
  | Binary (e1, `Add, e2) -> compile_binary env "binaryAdd" e1 e2
  | Binary (e1, `Sub, e2) -> compile_binary env "binarySub" e1 e2
  | Binary (e1, `Mult, e2) -> compile_binary env "binaryMult" e1 e2
  | Binary (e1, `Div, e2) -> compile_binary env "binaryDiv" e1 e2
  | Binary (e1, `And, e2) -> compile_binary env "binaryAnd" e1 e2
  | Binary (e1, `Or, e2) -> compile_binary env "binaryOr" e1 e2
  | Binary (e1, `Land, e2) -> compile_binary env "binaryLand" e1 e2
  | Binary (e1, `Lor, e2) -> compile_binary env "binaryLor" e1 e2
  | Binary (e1, `Xor, e2) -> compile_binary env "binaryXor" e1 e2
  | Binary (e1, `Lsl, e2) -> compile_binary env "binaryLsl" e1 e2
  | Binary (e1, `Lsr, e2) -> compile_binary env "binaryLsr" e1 e2
  | Binary (e1, `Lt, e2) -> compile_binary env "binaryLt" e1 e2
  | Binary (e1, `Le, e2) -> compile_binary env "binaryLe" e1 e2
  | Binary (e1, `Gt, e2) -> compile_binary env "binaryGt" e1 e2
  | Binary (e1, `Ge, e2) -> compile_binary env "binaryGe" e1 e2
  | Unary (Not, e) -> compile_unary env "unaryNot" e
  | Unary (Lnot, e) -> compile_unary env "unaryLnot" e
  | Unary (Neg, e) -> compile_unary env "unaryNeg" e
  | Unary (Pos, e) -> compile_unary env "unaryPos" e
  | If (e1, e2, e3) ->
      make_call (Symbol "if_")
        [
          Symbol callstack_varname;
          compile_expr env e1;
          compile_expr env e2;
          compile_expr env e3;
        ]
  | Function (params, body) ->
      with_binds env (params |> List.map fst) @@ fun () ->
      let binds =
        params
        |> List.mapi @@ fun i (id, e) ->
           ( varname env id,
             make_call (Symbol "functionParam")
               [
                 Symbol callstack_varname;
                 Symbol "args";
                 IntLiteral i;
                 StringLiteral id;
                 (match e with
                 | None -> Symbol "Nothing"
                 | Some e -> make_call (Symbol "Just") [ compile_expr env e ]);
               ] )
      in
      make_call (Symbol "Function")
        [
          IntLiteral (List.length params);
          Function
            ("cs", Function ("args", make_let binds (compile_expr env body)));
        ]
  | Call
      ( ({ v = ArrayIndex ({ v = Var std; _ }, { v = String name; _ }); _ } as e),
        positional,
        named )
    when std = "$std" || (std = "std" && env.is_stdjsonnet) ->
      compile_with_loc env e0.loc
        (match compile_builtin_std name with
        | Ok func ->
            make_call (Symbol func)
              [
                Symbol callstack_varname; compile_call_args env positional named;
              ]
        | Error _ -> compile_generic_call env (e, positional, named))
  | Call call -> compile_with_loc env e0.loc (compile_generic_call env call)
  | Error e ->
      compile_with_loc env e0.loc
        (make_call (Symbol "error'")
           [ Symbol callstack_varname; compile_expr env e ])
  | Local (binds, body) ->
      with_binds env (binds |> List.map fst) @@ fun () ->
      let bindings =
        binds |> List.map (fun (id, e) -> (varname env id, compile_expr env e))
      in
      make_let bindings (compile_expr env body)
  | Self -> make_call (Symbol "Object") [ List []; Symbol (varname env "self") ]
  | Var id -> (
      match Hashtbl.find env.vars id with
      | exception _ -> failwith ("var not found: " ^ id)
      | { name_in_target; desc = VarUnknown } -> Symbol name_in_target
      | { name_in_target; desc = VarObjectLocal (self, super) } ->
          make_call (Symbol name_in_target) [ Symbol self; Symbol super ])
  | InSuper e ->
      make_call (Symbol "inSuper")
        [
          Symbol callstack_varname;
          Symbol (varname env "super");
          compile_expr env e;
        ]
  | SuperIndex e ->
      make_call (Symbol "superIndex")
        [
          Symbol callstack_varname;
          Symbol (varname env "super");
          compile_expr env e;
        ]
  | Object { binds; assrts; fields } ->
      let fields (* compile keys with outer env *) =
        fields
        |> List.map (fun (e1, b, Syntax.H h, e2) ->
               ( Syntax.gensym () (* variable name for e1 *),
                 compile_expr env e1,
                 b,
                 h,
                 e2 ))
      in

      with_binds env
        ("self" :: "super"
        :: ((fields |> List.map (fun (k, _, _, _, _) -> k))
           @ (binds |> List.map fst)))
      @@ fun () ->
      binds
      |> List.iter (fun (id, _) ->
             set_var_desc env id
               (VarObjectLocal (varname env "self", varname env "super")));

      let bindings1 =
        fields |> List.map (fun (id, e1, _, _, _) -> (varname env id, e1))
      in
      let bindings2 =
        binds
        |> List.map (fun (id, e) ->
               let e =
                 Haskell.Function
                   ( varname env "self",
                     Function (varname env "super", compile_expr env e) )
               in
               (varname env id, e))
      in

      let assrts =
        Haskell.List
          (assrts
          |> List.map (fun e ->
                 Haskell.Function
                   ( varname env "self",
                     Function (varname env "super", compile_expr env e) )))
      in

      let fields =
        fields |> List.rev
        |> List.fold_left
             (fun acc (e1_key, _, plus, h, e2) ->
               let e2 = compile_expr env e2 in
               make_call (Symbol "objectField")
                 [
                   Symbol callstack_varname;
                   IntLiteral h;
                   Symbol (varname env e1_key);
                   Haskell.Function
                     ( varname env "self",
                       Function
                         ( varname env "super",
                           if plus then
                             make_call (Symbol "objectFieldPlusValue")
                               [
                                 Symbol callstack_varname;
                                 Symbol (varname env "super");
                                 Symbol (varname env e1_key);
                                 e2;
                               ]
                           else e2 ) );
                   acc;
                 ])
             (Symbol "emptyObjectFields")
      in
      let fields = make_call (Symbol "fillObjectCache") [ fields ] in

      make_let bindings1
        (make_let bindings2 (make_call (Symbol "Object") [ assrts; fields ]))
  | ObjectFor (e1, e2, x, e3) ->
      let compiled_e3 (* with env *) = compile_expr env e3 in
      with_binds env [ x ] @@ fun () ->
      let compiled_e1 (* with env + x *) = compile_expr env e1 in
      with_binds env [ "self"; "super" ] @@ fun () ->
      let compiled_e2 (* with env + x, self *) = compile_expr env e2 in
      make_call (Symbol "Object")
        [
          List [];
          make_call (Symbol "objectFor")
            [
              Symbol callstack_varname;
              Function
                ( "acc",
                  Function
                    ( varname env x,
                      make_call (Symbol "objectField")
                        [
                          Symbol callstack_varname;
                          IntLiteral 1;
                          compiled_e1;
                          Function
                            ( varname env "self",
                              Function (varname env "super", compiled_e2) );
                          Symbol "acc";
                        ] ) );
              compiled_e3;
            ];
        ]
  | Import file_path ->
      let import_id = get_import_id `Import file_path in
      make_call
        (Symbol (varname env import_id))
        [ Symbol "importedData"; Symbol callstack_varname ]
  | (Importbin file_path | Importstr file_path) as node ->
      let import_id =
        get_import_id
          (match node with
          | Import _ -> `Import
          | Importbin _ -> `Importbin
          | Importstr _ -> `Importstr
          | _ -> assert false)
          file_path
      in
      make_call (Symbol (varname env import_id)) [ Symbol "importedData" ]

and compile_unary env sym e =
  make_call (Symbol sym) [ Symbol callstack_varname; compile_expr env e ]

and compile_binary env sym e1 e2 =
  make_call (Symbol sym)
    [ Symbol callstack_varname; compile_expr env e1; compile_expr env e2 ]

and compile_generic_call env (e, positional, named) =
  let funcname = match e.v with Syntax.Core.Var s -> s | _ -> "anonymous" in
  compile_with_name env funcname
    (make_call
       (make_call (Symbol "getFunction")
          [ Symbol callstack_varname; compile_expr env e ])
       [ Symbol callstack_varname; compile_call_args env positional named ])

and compile_call_args env positional named =
  Tuple
    [
      List (List.map (compile_expr env) positional);
      make_call (Symbol "HashMap.fromList")
        [
          List
            (List.map
               (fun (id, v) ->
                 Haskell.Tuple [ StringLiteral id; compile_expr env v ])
               named);
        ];
    ]

let get_ext_code_id key = "extCode/" ^ key

let compile ?multi ?(string = false) ?(target = `Main) root_prog_path progs bins
    strs ext_codes =
  let env = { vars = Hashtbl.create 0; is_stdjsonnet = target = `Stdjsonnet } in

  let bind_ids =
    "$std"
    :: ((progs
        |> List.map (fun (real_path, _, _) -> get_import_id `Import real_path))
       @ (bins |> List.map (get_import_id `Importbin))
       @ (strs |> List.map (get_import_id `Importstr))
       @ (ext_codes |> List.map fst |> List.map get_ext_code_id))
  in

  with_binds env bind_ids @@ fun () ->
  let progs_bindings =
    progs
    |> List.map (fun (real_path, path, e) ->
           ( varname env (get_import_id `Import real_path),
             Haskell.Let
               ( [
                   ( varname env "$std",
                     make_call (Symbol "makeStd") [ StringLiteral path ] );
                 ],
                 compile_expr env e ) ))
  in
  let bins_bindings =
    bins
    |> List.map (fun path ->
           ( varname env (get_import_id `Importbin path),
             make_call (Symbol "readBin") [ StringLiteral path ] ))
  in
  let strs_bindings =
    strs
    |> List.map (fun path ->
           ( varname env (get_import_id `Importstr path),
             make_call (Symbol "readStr") [ StringLiteral path ] ))
  in

  let importedDataFields =
    (bins_bindings |> List.map fst) @ (strs_bindings |> List.map fst)
    |> List.map (fun name -> Printf.sprintf "%s :: Value" name)
    |> String.concat ", "
  in

  let progs_flatten =
    progs_bindings
    |> List.map (fun (id, e) ->
           Printf.sprintf
             "%s :: ImportedData -> CallStack -> Value\n%s importedData %s = %s"
             id id callstack_varname (Haskell.show_expr e))
    |> String.concat "\n"
  in

  match target with
  | `Main ->
      let v =
        make_call
          (Symbol (varname env (get_import_id `Import root_prog_path)))
          [
            Haskell.RecordConstruction
              ( "MkImportedData",
                bins_bindings @ strs_bindings
                |> List.map (fun (id, _) -> (id, Haskell.Symbol id)) );
          ]
      in
      let v =
        if Option.is_some multi then
          let target_dir =
            if Filename.is_relative (Option.get multi) then
              Filename.concat (Sys.getcwd ()) (Option.get multi)
            else Option.get multi
          in
          make_call (Symbol "mainMulti")
            [ StringLiteral target_dir; BoolLiteral string; v ]
        else if string then make_call (Symbol "mainString") [ v ]
        else make_call (Symbol "mainNormal") [ v ]
      in
      let main =
        Haskell.Do
          (List.flatten
             [
               bins_bindings |> List.map (fun x -> Haskell.Assign x);
               strs_bindings |> List.map (fun x -> Haskell.Assign x);
               [ v ];
             ])
        |> Haskell.show_expr
      in

      Printf.sprintf
        {|module Main (main) where

import Common
import qualified Stdjsonnet
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.HashMap.Lazy as HashMap

data ImportedData = MkImportedData { %s }

makeStd :: String -> Value
makeStd thisFile =
  case Stdjsonnet.v [] of
    (Object _ fields) -> Object [] $ fillObjectCache $ insertStd fields

%s = makeStd ""

%s

main :: IO ()
main = %s
|}
        importedDataFields (varname env "$std") progs_flatten main
  | _ ->
      Printf.sprintf
        {|module Stdjsonnet where

import Common
import qualified Data.HashMap.Lazy as HashMap

%s = Object [] $ fillObjectCache $ insertStd emptyObjectFields

v :: CallStack -> Value
v = \%s -> %s
|}
        (varname env "$std") callstack_varname
        (progs |> List.hd
        |> (fun (_, _, s) -> s)
        |> compile_expr env |> Haskell.show_expr)
