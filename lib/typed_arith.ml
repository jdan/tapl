include Arith

type typ =
  | Bool
  | Nat

exception TypeError of string
let rec typ_of_t = function
  | True -> Bool                  (* T-TRUE *)
  | False -> Bool                 (* T-FALSE *)
  | If (cond, cons, alt) ->       (* T-IF *)
    if Bool = typ_of_t cond
    then (
      let t_cons = typ_of_t cons
      and t_alt = typ_of_t alt
      in
      if t_cons <> t_alt
      then raise (TypeError "T-IF expects both branches to be of the same type")
      else t_cons
    )
    else raise (TypeError "T-IF expects a Bool condition")
  | O -> Nat                      (* T-ZERO *)
  | Succ n ->                     (* T-SUCC *)
    if Nat = typ_of_t n
    then Nat
    else raise (TypeError "T-SUCC expects a Nat")
  | Pred n ->                     (* T-PRED *)
    if Nat = typ_of_t n
    then Nat
    else raise (TypeError "T-PRED expects a Nat")
  | IsZero n ->                   (* T-ISZERO *)
    if Nat = typ_of_t n
    then Bool
    else raise (TypeError "T-ISZERO expects a Nat")

let%test _ = Bool = typ_of_t True
let%test _ = Bool = typ_of_t False
let%test _ = Nat = typ_of_t (If (True, O, O))
let%test _ = Bool = typ_of_t (If (True, True, False))
let%test _ =
  try
    ignore (typ_of_t (If (O, True, False))); false
  with TypeError "T-IF expects a Bool condition" -> true
let%test _ =
  try
    ignore (typ_of_t (If (True, O, False))); false
  with TypeError "T-IF expects both branches to be of the same type" -> true
let%test _ = Nat = typ_of_t O
let%test _ = Nat = typ_of_t (Succ O)
let%test _ =
  try
    ignore (typ_of_t (Succ True)); false
  with TypeError "T-SUCC expects a Nat" -> true
let%test _ = Nat = typ_of_t (Pred O)
let%test _ = Nat = typ_of_t (Pred (Succ O))
let%test _ =
  try
    ignore (typ_of_t (Pred False)); false
  with TypeError "T-PRED expects a Nat" -> true
let%test _ = Bool = typ_of_t (IsZero O)
let%test _ = Bool = typ_of_t (IsZero (Succ O))
let%test _ =
  try
    ignore (typ_of_t (IsZero False)); false
  with TypeError "T-ISZERO expects a Nat" -> true
