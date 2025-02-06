include Frontend.Ast
include Oplot.Plt
include Oplot.Points

let to_plot = ref []

let dist (p1 : point) (p2 : point) =
  sqrt ((p1.x -. p2.x) ** 2. +. (p1.y -. p2.y) ** 2.)

let plot_point (p : point) =
  let plot =
    point_plot_f (fun _ -> p.y) p.x p.x
  in
  to_plot := plot :: color 0. 0. 0. :: !to_plot

let plot_line (p1 : point) (p2 : point) =
  let p =
    line_plot_f
      (fun x -> p1.y +. (x -. p1.x) *. (p2.y -. p1.y))
      p1.x p2.x ~pas:0.05
  in
  to_plot := p :: color 255. 255. 255. :: !to_plot

let plot_curve (p1 : point) (p2 : point) (p3 : point) =
  let p =
    line_plot_f
      (fun x -> sqrt(dist p1 p3 -. (x -. p3.x) ** 2.) +. p3.y)
      p1.x p2.x ~pas:0.05
  in
  to_plot := p :: color 255. 255. 255. :: !to_plot

let rec interpret_stmt_list (p : point) (sl : stmt list) : point =
  match sl with 
  | [] -> p
  | s :: rest ->
    let new_p = interpret_stmt p s in
    interpret_stmt_list new_p rest
and interpret_stmt (p : point) (s : stmt) : point =
  match s with
  | Translation (abs_u, abs_v) -> begin
      let u = Float.of_string abs_u in
      let v = Float.of_string abs_v in
      let new_point = point (p.x +. u, p.y +. v) in
      let () = plot_line p new_point in
      new_point
    end
  | Rotation (abs_u, abs_v, abs_theta) -> begin
      let u = Float.of_string abs_u in
      let v = Float.of_string abs_v in
      let theta = Float.of_string abs_theta in
      let cos_theta = cos theta in
      let sin_theta = sin theta in
      let translated_x = p.x -. u in
      let translated_y = p.y -. v in
      let rotated_x =
        translated_x *. cos_theta -. translated_y *. sin_theta in
      let rotated_y =
        translated_x *. sin_theta +. translated_y *. cos_theta in
      let new_point = point (rotated_x +. u, rotated_y +. v) in
      let () = plot_curve p new_point (point (u, v)) in
      new_point
    end
  | Iteration sl -> begin
      let n = Random.int 10 in
      iterate p sl n
    end
  | Or (sl1, sl2) -> begin
      let n = Random.bool () in
      if n then
        interpret_stmt_list p sl1
      else
        interpret_stmt_list p sl2
    end
and iterate (p : point) (sl : stmt list) (n : int) : point =
  if n = 0 then
    p
  else
    iterate (interpret_stmt_list p sl) sl (n - 1)

let init_set (AbstractSet (lower, upper)) : float =
  let lower_float = Float.of_string lower in
  let upper_float = Float.of_string upper in
  lower_float +. (Random.float (upper_float -. lower_float))

let interpret_init (Init (s1, s2)) : point =
  point (init_set s1, init_set s2)

let interpret (Program (i, sl)) : point =
  let p = interpret_init i in 
  let () = plot_point p in
  List.fold_left interpret_stmt p sl