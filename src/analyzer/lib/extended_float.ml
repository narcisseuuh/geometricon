type mathematical_float =
  | F of float
  | INF
  | N_INF

let add x y =
  match (x, y) with
  | (F a, F b) -> F (a +. b)
  | (INF, _) | (_, INF) -> INF
  | (N_INF, _) | (_, N_INF) -> N_INF

let (<+>) x y =
  match x with
  | N_INF | INF -> x
  | F x' -> F (x' +. y)

let sub x y =
  match (x, y) with
  | (F a, F b) -> F (a -. b)
  | (INF, F _) | (INF, N_INF) -> INF
  | (N_INF, F _) | (N_INF, INF) -> N_INF
  | (F _, INF) -> N_INF
  | (F _, N_INF) -> INF
  | (INF, INF) | (N_INF, N_INF) -> failwith "undefined"
  
let mul x y =
  match (x, y) with
  | (F a, F b) -> F (a *. b)
  | (INF, F a) | (F a, INF) -> if a = 0.0 then failwith "undefined" else INF
  | (N_INF, F a) | (F a, N_INF) -> if a = 0.0 then failwith "undefined" else N_INF
  | (INF, INF) | (N_INF, N_INF) -> INF
  | (INF, N_INF) | (N_INF, INF) -> N_INF

let div x y =
  match (x, y) with
  | (F a, F b) -> if b = 0.0 then failwith "division by zero" else F (a /. b)
  | (INF, F a) -> if a = 0.0 then failwith "undefined" else INF
  | (N_INF, F a) -> if a = 0.0 then failwith "undefined" else N_INF
  | (F _, INF) | (F _, N_INF) -> F 0.0
  | (INF, INF) | (N_INF, N_INF) -> failwith "undefined"
  | (INF, N_INF) | (N_INF, INF) -> failwith "undefined"

let compare x y =
  match (x, y) with
  | (F a, F b) -> compare a b
  | (INF, INF) | (N_INF, N_INF) -> 0
  | (INF, _) | (_, N_INF) -> 1
  | (N_INF, _) | (_, INF) -> -1

let equal x y =
  match (x, y) with
  | (F a, F b) -> a = b
  | (INF, INF) | (N_INF, N_INF) -> true
  | _ -> false

let less_than x y =
  match (x, y) with
  | (F a, F b) -> a < b
  | (N_INF, _) | (_, INF) -> true
  | (INF, _) | (_, N_INF) -> false

let greater_than x y =
  match (x, y) with
  | (F a, F b) -> a > b
  | (INF, _) | (_, N_INF) -> true
  | (N_INF, _) | (_, INF) -> false

let min x y =
  match (x, y) with
  | (F a, F b) -> if a < b then F a else F b
  | (N_INF, _) | (_, N_INF) -> N_INF
  | (INF, _) -> y
  | (_, INF) -> x

let max x y =
  match (x, y) with
  | (F a, F b) -> if a > b then F a else F b
  | (INF, _) | (_, INF) -> INF
  | (N_INF, _) -> y
  | (_, N_INF) -> x