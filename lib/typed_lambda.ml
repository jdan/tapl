(* TAPL chapter 9 *)

type typ =
  | Bool
  | Function of typ * typ
  | Unit

type exp =
  | True
  | False
  | Var of string
  | Abstr of string * typ * exp
  | App of exp * exp
  | Unit

type value =
  | AbstrValue of string * typ * exp
  | TrueValue
  | FalseValue
  | UnitValue

type env = (string * value) list
type context = (string * typ) list

let empty_context = []

let rec string_of_typ = function
  | Bool -> "Bool"
  | Function (l, r) -> string_of_typ l ^ " -> " ^ string_of_typ r
  | Unit -> "unit"

exception TypeError of string
let rec typ_of_exp context = function
  | True -> Bool
  | False -> Bool
  | Var binding -> List.assoc binding context
  | Abstr (binding, t, exp) ->
    Function (t, typ_of_exp ((binding, t) :: context) exp)
  | App (e1, e2) ->
    let e2_typ = typ_of_exp context e2
    in (
      match typ_of_exp context e1 with
      | Function (e1_left, e1_right) ->
        if e1_left = e2_typ
        then e1_right
        else raise (TypeError (
            "App: Expected argument to have type " ^
            string_of_typ e1_left ^
            ". Got: " ^
            string_of_typ e2_typ
          ))
      | t -> raise (TypeError (
          "App: Expected function. Got: " ^ string_of_typ t
        ))
    )
  | Unit -> Unit

let%test _ =
  Function (Bool, Bool) = typ_of_exp [("x", Bool)] (Abstr ("y", Bool, (Var "x")))
let%test _ =
  Bool = typ_of_exp [("y", Function (Bool, Bool))] (App (Var "y", True))
let%test _ =
  Bool = typ_of_exp [("x", Bool); ("y", Function (Bool, Bool))] (App (Var "y", Var "x"))
let%test _ =
  try
    ignore (typ_of_exp [("x", Bool)] (App (Var "x", Var "x"))); false
  with TypeError "App: Expected function. Got: Bool" -> true
let%test _ =
  try
    ignore (typ_of_exp [("x", Function (Function (Bool, Bool), Bool))] (App (Var "x", True))); false
  with TypeError "App: Expected argument to have type Bool -> Bool. Got: Bool" -> true

let rec string_of_exp = function
  | True -> "True"
  | False -> "False"
  | Var name -> name
  | Abstr (name, t, body) -> "λ" ^ name ^ ":" ^ string_of_typ t ^ "."  ^ string_of_exp body
  | App (l, r) -> "(" ^ string_of_exp l ^ " " ^ string_of_exp r ^ ")"
  | Unit -> "()"

let%test _ =
  "λx:Bool -> Bool.λy:Bool.(x y)" =
  string_of_exp (Abstr ("x", Function (Bool, Bool), (Abstr ("y", Bool, (App (Var "x", Var "y"))))))

let%test _ = "λx:unit.()" = string_of_exp (Abstr ("x", Unit, Unit))
