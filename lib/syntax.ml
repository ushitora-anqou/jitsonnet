type expr =
  | Number of float
  | Var of id
  | Null
  | True
  | False
  | Self
  | Dollar
  | String of string
  | Object of objinside
  | Array of expr list
  | ArrayFor of (expr * forspec * compspec)

and id = string

and objinside =
  | ObjectMemberList of member list
  | ObjectFor of
      (objlocal list * expr * expr * objlocal list * forspec * compspec)

and member =
  | MemberObjlocal of objlocal
  | MemberAssert of assert_
  | MemberField of field

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
  | FieldnameID of id
  | FieldnameString of string
  | FieldnameExpr of expr

and assert_ = expr * expr option
and bind = Bind of (id * expr) | BindFunc of (id * params * expr)
and params = param list
and param = id * expr option [@@deriving show]

type program = { expr : expr }

exception General_parse_error of string
