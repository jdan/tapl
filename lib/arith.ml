(* TAPL chapter 3 *)

type t =
  | True
  | False
  | If of t * t * t
  | O
  | Succ of t
  | Pred of t
  | IsZero of t

type nv =
  | NumericO
  | NumericSucc of nv

type v =
  | TrueValue
  | FalseValue
  | Numeric of nv

exception EvalError
let rec eval = function
  | True -> TrueValue
  | False -> FalseValue
  | If (cond, cons, alt) ->
    if eval cond = TrueValue
    then eval cons
    else eval alt
  | O -> Numeric (NumericO)
  | Succ n -> (
      match eval n with
      | Numeric nv -> Numeric (NumericSucc nv)
      | _ -> raise EvalError
    )
  | Pred n -> (
      match eval n with
      | Numeric (NumericSucc nv) -> Numeric nv
      | Numeric NumericO -> Numeric NumericO
      | _ -> raise EvalError
    )
  | IsZero n -> (
      match eval n with
      | Numeric NumericO -> TrueValue
      | Numeric _ -> FalseValue
      | _ -> raise EvalError
    )

let rec int_of_numeric_value = function
  | NumericO -> 0
  | NumericSucc n -> int_of_numeric_value n + 1

let%test _ = 0 = int_of_numeric_value NumericO
let%test _ = 1 = int_of_numeric_value (NumericSucc NumericO)
let%test _ = 2 = int_of_numeric_value (NumericSucc (NumericSucc NumericO))

let string_of_value = function
  | TrueValue -> "true"
  | FalseValue -> "false"
  | Numeric nv -> int_of_numeric_value nv |> string_of_int

let%test _ = "3" = (eval (
    If ( (IsZero (Pred (Succ O)))
       , Succ (Succ (Succ O))
       , O
       )
  ) |> string_of_value)
