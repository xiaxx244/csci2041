let circle_circum_v1 radius=2.0 *. 3.1415 *.radius

let circle_circum_v2 radius=
  let pi=3.1415 in
    2.0 *. pi *. radius

let rec product xs=
  match xs with
  | [] -> 1
  | p::rest -> p*product rest

let rec sum_sqrdiffs xs=
  match xs with
  | []| [_]-> raise(Invalid_argument("Invalid list length"))
  | x1::(x2::[]) -> (x1-x2) * (x1-x2)
  | x1::(x2::rest)-> (x1-x2) * (x1-x2)+sum_sqrdiffs (x2::rest)

let distance (x1,y1) (x2,y2)=
  sqrt((x1 -.x2)**2.+.(y1-.y2)**2.)

let triangle_perimeter (x1,y1) (x2,y2) (x3,y3)=
  (distance (x1,y1) (x2,y2)) +. (distance (x2,y2) (x3,y3)) +.(distance (x3,y3) (x1,y1))
