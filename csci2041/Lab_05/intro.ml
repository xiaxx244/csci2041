(*work with Xingtong Zhang,Yuze Jiang,Youya Xia*)

let even x=if x mod 2=0 then true else false

let rec euclid a b=if (a=0 && b!=0) then (abs(b))
                  else if (b=0 && a !=0) then (abs(a))
                  else if (a<0 || b<0) then euclid (abs(a)) (abs(b))
                  else if a=b then a
                  else if a<b then euclid a (b-a)
                  else euclid (a-b) b


let frac_simplify (x,y)=if(y=0) then raise(Failure "Input denominator cannot be zero")
                        else if (x=0 && y!=0) then (0,1)
                        else if (x*y>0) then ((abs(x))/(euclid x y),(abs(y))/(euclid x y))
                        else (x/(euclid x y),y/(euclid x y))

(*change the indenting way to make my code nicer*)
let rec max : int list-> int
  =fun xs->
  match xs with
  |[]->raise (Failure "Input list must not be empty")
  |[hd]->hd
  |x1::(x2::rest)->if x1>x2 then max (x1::rest) else max (x2::rest)

(*change the indenting way to make my code look nicer*)
let rec take num xs=
  match xs with
  |[]->if num >0 then raise(Failure "Invalid argument") else[]
  |hd::rest->if num<=0 then [] else hd::take (num-1) rest
