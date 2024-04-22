type unaryop = Lnot | Neg | Not | Pos [@@deriving show]

type binop =
  | Add
  | And
  | Div
  | Equal
  | Ge
  | Gt
  | In
  | Land
  | Le
  | Lor
  | Lsl
  | Lsr
  | Lt
  | Mod
  | Mult
  | NotEqual
  | Or
  | Sub
  | Xor
[@@deriving show]

type expr =
  | Array of expr list
  | ArrayFor of (expr * forspec * compspec)
  | ArrayIndex of (expr * expr)
  | ArraySlice of (expr * expr option * expr option * expr option)
  | Assert of (assert_ * expr)
  | Binary of (expr * binop * expr)
  | Call of (expr * args)
  | Dollar
  | False
  | Function of (params * expr)
  | If of (expr * expr * expr option)
  | Import of string
  | Importstr of string
  | Local of (bind list * expr)
  | Null
  | Number of float
  | Object of objinside
  | ObjectSeq of (expr * objinside)
  | Select of (expr * id)
  | Self
  | String of string
  | SuperIndex of expr
  | SuperSelect of id
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
