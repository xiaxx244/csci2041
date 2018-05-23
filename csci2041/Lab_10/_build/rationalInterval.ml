open Intervals

module Rational_comparable : (Comparable with type t = int*int) = struct
  type t = int*int

  let compare (x,y) (a,b)=
    if (((float_of_int x) /.(float_of_int y))
      -.((float_of_int a) /.(float_of_int b)))>0.0 then 1
    else (-1)

  let rec euclid a b=if (a=0 && b!=0) then (abs(b))
                    else if (b=0 && a !=0) then (abs(a))
                    else if (a<0 || b<0) then euclid (abs(a)) (abs(b))
                    else if a=b then a
                    else if a<b then euclid a (b-a)
                    else euclid (a-b) b

  let frac_simplify (x,y)=if(y=0)
                then raise(Failure "Input denominator cannot be zero")
                          else if (x=0 && y!=0) then (0,1)
                          else if (x*y>0)
                          then ((abs(x))/(euclid x y),(abs(y))/(euclid x y))
                          else (x/(euclid x y),y/(euclid x y))

  let to_string (x,y)= let  temp=frac_simplify (x,y)
                        in
                        match temp with
                        |(a,b)->(string_of_int a)^"/"^(string_of_int b)
              
end

module Rational_interval = Make_interval(Rational_comparable)



(* The following line now works. *)
let i = Rational_interval.create (3,5) (4,6)
