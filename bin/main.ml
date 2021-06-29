open Lib
open Arith

let () =
  eval (
    If ( (IsZero (Pred (Succ O)))
       , Succ (Succ (Succ O))
       , O
       )
  ) |> string_of_value |> print_endline
