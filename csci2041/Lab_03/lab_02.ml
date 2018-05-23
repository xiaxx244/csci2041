(*TODO Youya Xia; Yuze Jiang ;Xintong Zhang;Dong Liang*)

(*change radius to r to shorten my code*)
let circle_circum_v1 r=2.0 *. 3.1415 *.r

(*change radius to r to shorten my code and use a single line for 'in' and let pi and the final math expression should have the same indentation since it can help me have a clear and good coding style*)
let circle_circum_v2 r=
  let pi=3.1415
  in
  2.0 *. pi *. r

(*since I have noticed this case during lab_02, so I do not need to consider this case during this lab*)
let rec product xs=
  match xs with
  | [] -> 1
  | p::rest -> p*product rest

(*change the type of expcetion from Invalid_argument to Failure since this is not an Invalid_argument errror ,it should be catched by the exception of type Failure*)
let rec sum_sqrdiffs xs=
  match xs with
  | []| [_]-> raise(Failure "sum_sqrdiffs input list needs at least two elements")
  | x1::(x2::[]) -> (x1-x2) * (x1-x2)
  | x1::(x2::rest)-> (x1-x2) * (x1-x2)+sum_sqrdiffs (x2::rest)

let distance (x1,y1) (x2,y2)=
  sqrt((x1 -.x2)**2.+.(y1-.y2)**2.)

(*use v1,v2 and v3 to represent the three points instead of using the coordinate directly since ocaml can infer the type for me ,which can help me shoreten my code and have a good coding style*)
let triangle_perimeter v1 v2 v3=
  (distance v1 v2) +. (distance v2 v3) +.(distance v3 v1)
