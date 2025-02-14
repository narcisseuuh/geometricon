include Domain

type mathematical_float =
  | F of float
  | INF
  | N_INF

let max m1 m2 =
  match m1, m2 with
  | INF, _ | _, INF -> INF
  | N_INF, _ -> m2
  | _, N_INF -> m1
  | F x1, F x2 -> F (max x1 x2)

let min m1 m2 =
  match m1, m2 with
  | N_INF, _ | _, N_INF -> N_INF
  | INF, _ -> m2
  | _, INF -> m1
  | F x1, F x2 -> F (min x1 x2)

let (<=) m1 m2 =
  match m1, m2 with
  | N_INF, _-> true
  | _, N_INF -> false
  | INF, _ -> false
  | _, INF -> true
  | F x1, F x2 -> x1 <= x2

let (<+>) x y =
  (* In practice, adding N_INF and INF cannot happen, so I don't care ^^ *)
  match x with
  | INF -> INF
  | N_INF -> N_INF
  | F xx -> F (xx +. y)

let rotate_point u v theta (x, y) =
  match x, y with 
  | INF, _ | _, INF -> (x, y)
  | N_INF, _ | _, N_INF -> (x, y)
  | F x, F y ->
    let x' = x -. u in
    let y' = y -. v in
    let cos_t = cos theta in
    let sin_t = sin theta in
    let x'' = x' *. cos_t -. y' *. sin_t in
    let y'' = x' *. sin_t +. y' *. cos_t in
    (F (x'' +. u), F (y'' +. v))

type concrete_set =
  | ConcreteSet of mathematical_float * mathematical_float
  | BOTTOM

let rotate_rectangle s1 s2 u v theta =
  match s1, s2 with
  | BOTTOM, _ | _, BOTTOM -> (BOTTOM, BOTTOM)
    (* In case one set is bottom, the rectangle is empty... *)
  | ConcreteSet (a, b), ConcreteSet (c, d) ->
    let corners = 
      [(a, c); (a, d); (b, c); (b, d)] |> List.filter 
        (fun (x, y) ->
          match x, y with
          | N_INF, _ | INF, _ | _, N_INF | _, INF -> true
          | _ -> false)
    in
    let rotated_corners = List.map (rotate_point u v theta) corners in
    let xs, ys = List.split rotated_corners in
    let min_x =
      if List.exists ((=) N_INF) [a; b] then
        N_INF 
      else 
        List.fold_left min (List.hd xs) xs
    in
    let max_x =
      if List.exists ((=) INF) [a; b] then
        INF 
      else 
        List.fold_left max (List.hd xs) xs
    in
    let min_y =
      if List.exists ((=) N_INF) [c; d] then
        N_INF 
      else 
        List.fold_left min (List.hd ys) ys
    in
    let max_y = 
      if List.exists ((=) INF) [c; d] then
        INF 
      else 
        List.fold_left max (List.hd ys) ys 
    in
    (ConcreteSet (min_x, max_x), ConcreteSet (min_y, max_y))

let subset_concrete (s1 : concrete_set)(s2 : concrete_set) : bool =
  match s1, s2 with
  | BOTTOM, _ -> true
  | _, BOTTOM -> false
  | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
    (x1 <= y1) && (x2 <= y2)

let intersection_concrete (s1 : concrete_set)(s2 : concrete_set) : concrete_set =
  match s1, s2 with
  | BOTTOM, _ | _, BOTTOM -> BOTTOM
  | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
    ConcreteSet (max x1 y1, min x2 y2)

module Interval : Domain_t =
struct
  type t = concrete_set * concrete_set

  let init x =
    match x with
    | Init (AbstractSet (x1, x2), AbstractSet (y1, y2))
      -> (ConcreteSet (F (float_of_string x1), F (float_of_string x2)),
          ConcreteSet (F (float_of_string y1), F (float_of_string y2)))

  let to_t (s1, s2) = init (Init (s1, s2))

  let bottom () = (BOTTOM, BOTTOM)
  let is_bottom (x : t) : bool =
    match x with
    | (BOTTOM, BOTTOM) -> true
    | _ -> false

  let join (x1 : t)(x2 : t) : t =
    let join_sets (s1 : concrete_set)(s2 : concrete_set) : concrete_set =
      match s1, s2 with
      | BOTTOM, x | x, BOTTOM -> x
      | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
        ConcreteSet (min x1 y1, max x2 y2)
    in
    match x1, x2 with
    | (x11, x12), (x21, x22) ->
      (join_sets x11 x21, join_sets x12 x22)

  let widen (x1 : t)(x2 : t) : t =
    let widen_sets (s1 : concrete_set)(s2 : concrete_set) : concrete_set =
      match s1, s2 with
      | BOTTOM, _ | _, BOTTOM -> BOTTOM
      | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
        if x1 <= y1 then
          begin
            if x2 <= y2 then
              ConcreteSet (x1, INF)
            else
              ConcreteSet (N_INF, x2)
          end
        else
          ConcreteSet (N_INF, INF)
    in match x1, x2 with
    | (x11, x12), (y11, y12) ->
      (widen_sets x11 y11, widen_sets x12 y12)

  let translate ~(u : float)~(v : float)(x : t) : t =
    match x with 
    | BOTTOM, BOTTOM -> x
    | ConcreteSet (x1, x2), BOTTOM ->
      (ConcreteSet (x1 <+> u, x2 <+> u), BOTTOM)
    | BOTTOM, ConcreteSet (y1, y2) ->
      (BOTTOM, ConcreteSet (y1 <+> v, y2 <+> v))
    | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
      (ConcreteSet (x1 <+> u, x2 <+> u), ConcreteSet (y1 <+> v, y2 <+> v))
  
  let rotate ~(u : float)~(v : float)~(theta : float)(x : t) : t =
    match x with
    | s1, s2 ->
      rotate_rectangle s1 s2 u v theta

  let subset (x1 : t)(x2 : t) : bool =
    match x1, x2 with
    | (x11, x12), (x21, x22) ->
      (subset_concrete x11 x21) && (subset_concrete x12 x22)

  let meet (x1 : t)(x2 : t) : t =
    match x1, x2 with
    | (x11, x12), (x21, x22) ->
      (intersection_concrete x11 x21, intersection_concrete x12 x22)

  let print (fmt : Format.formatter)(x : t) : unit =
    let print_mf fmt mf =
      match mf with
      | N_INF -> Format.fprintf fmt "-infty"
      | INF -> Format.fprintf fmt "infty"
      | F f -> Format.fprintf fmt "%a" Format.pp_print_float f
    in
    let print_fs fmt fs =
      match fs with 
      | BOTTOM -> Format.fprintf fmt "{}"
      | ConcreteSet (x1, x2) ->
        Format.fprintf fmt "[%a, %a]" print_mf x1 print_mf x2
    in
    match x with
    | (s1, s2) ->
      Format.fprintf fmt "(%a, %a)" print_fs s1 print_fs s2
end