include Domain
include Extended_float

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
  | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
    let (rotated_x1, rotated_x2) = rotate_point u v theta (x1, x2) in
    let (rotated_y1, rotated_y2) = rotate_point u v theta (y1, y2) in
    (ConcreteSet (Extended_float.min rotated_x1 rotated_x2, Extended_float.max rotated_x1 rotated_x2),
    ConcreteSet (Extended_float.min rotated_y1 rotated_y2, Extended_float.max rotated_y1 rotated_y2))

let subset_concrete (s1 : concrete_set)(s2 : concrete_set) : bool =
  match s1, s2 with
  | BOTTOM, _ -> true
  | _, BOTTOM -> false
  | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
    (Extended_float.compare x1 y1 <= 0) && (Extended_float.compare x2 y2 >= 0)

let intersection_concrete (s1 : concrete_set)(s2 : concrete_set) : concrete_set =
  match s1, s2 with
  | BOTTOM, _ | _, BOTTOM -> BOTTOM
  | ConcreteSet (x1, x2), ConcreteSet (y1, y2) ->
    if max x1 y1 <= min x2 y2 then
      ConcreteSet (max x1 y1, min x2 y2)
    else
      BOTTOM

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
      | BOTTOM, x | x, BOTTOM -> x
      | ConcreteSet (a, b), ConcreteSet (a', b') ->
        let lower =
          if Extended_float.less_than a' a then
            N_INF
          else 
            a
        in
        let upper =
          if Extended_float.greater_than b' b then
            INF
          else
            b
        in ConcreteSet (lower, upper)
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