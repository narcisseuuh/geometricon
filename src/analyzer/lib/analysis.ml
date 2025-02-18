include Frontend.Ast
include Domain

let loop_unroll = ref 3
let target_set =
    ref (AbstractSet ("-1", "0"), AbstractSet ("0", "1"))

module Interprete(D : Domain_t) =
struct
  type t = D.t

  let set_t = D.to_t !target_set
  let results = ref []

  let add_result (x : t)(s : stmt) : unit =
    if not (D.is_bottom (D.meet x set_t)) then
      results := (s, x) :: !results

  let rec show_results () =
    match !results with
    | [] -> ()
    | (s, x) :: rest ->
      begin
        Format.fprintf Format.std_formatter
        "@[<v 2>Result:@,@[<hov 2>State:@ %a@]@,\
        @[<hov 2>Intersects with target set:@ %a@]@,\
        @[<hov 2>In statement:@ %a@]@]\n"
        D.print x D.print set_t print_stmt s;
        results := rest;
        show_results ()
      end

  let rec eval_stmt (a : t)(s : stmt) : t =
    match s with
    | Translation (u_str, v_str) ->
      let u = float_of_string u_str in
      let v = float_of_string v_str in
      D.translate ~u ~v a
    | Rotation (u_str, v_str, theta_str) ->
      let u = float_of_string u_str in
      let v = float_of_string v_str in
      let theta = float_of_string theta_str in
      D.rotate ~u ~v ~theta a
    | Iteration s ->
      let rec unroll (n : int)(s :stmt list)(x : t) : t =
        if n = 0 then x
        else unroll (n - 1) s (D.join x (eval_stmt_list x s))
      in
      let rec fixpoint (f : t -> t)(x : t) =
        let fx = f x in
        if D.subset fx x then fx
        else fixpoint f (D.join x fx)
      in
      let unrolled = unroll !loop_unroll s a in
      fixpoint (fun x -> D.widen x (eval_stmt_list x s)) unrolled
    | Or (s1, s2) ->
      let analysis_lhs = eval_stmt_list a s1 in
      let analysis_rhs = eval_stmt_list a s2 in
      D.join analysis_lhs analysis_rhs
  and eval_stmt_list (a : t)(s_l : stmt list) : t =
    List.fold_left eval_stmt a s_l

  let eval_prog (Program (i, p) : program) : unit =
    let eval_and_show x_pre s =
      let x_post = eval_stmt x_pre s in
      let () = add_result x_post s in
      x_post
    in let _ = List.fold_left eval_and_show (D.init i) p
    in show_results ()
end