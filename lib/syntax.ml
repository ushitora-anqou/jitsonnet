type expr =
  | Number of float
  | ID of id
  | Null
  | True
  | False
  | Self
  | Dollar
  | String of string
  | Object of objinside

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
  | Field of (fieldname * h * expr)
  | FieldFunc of (fieldname * params option * h * expr)

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

and assert_ = Assert of (expr * expr option)
and bind = Bind of (id * expr) | BindFunc of (id * params option * expr)
and params = param list
and param = id * expr option [@@deriving show]

type program = { expr : expr }
