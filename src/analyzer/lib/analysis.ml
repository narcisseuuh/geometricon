include Frontend.Ast
include Domain

let loop_unroll = ref 3

let widening_delay = ref 2

module Interprete(D : Domain_t) =
struct
  type t = D.t

  let curr_delay = ref 0
  let curr_unroll = ref 0

  let eval_stmt (a : t)(s : stmt) : t =
    let _r = match s with
    | Translation (_u, _v) ->
      ()
    | Rotation (_u, _v, _theta) ->
      ()
    | Iteration _s ->
      ()
    | Or (_s1, _s2) ->
      ()
    in a

  let eval_prog (Program (i, p) : program) : unit =
    let _ = List.fold_left eval_stmt (D.init i) p in
    ()
end