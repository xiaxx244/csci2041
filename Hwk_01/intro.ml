(*this function below is to determine if an integer is even or not*)
(*type:int -> bool*)
let even x=if x mod 2=0 then true else false

(*this function below is to find the gcd of two integers a and b based on math definition*)
(*since gcd is always positive,for negative input,just switch the sign and compute gcd abs(a) abs(b)*)
(*if a or b is 0,just return the absolute value of the one that is not 0*)
(*type:int -> int -> int*)
let rec euclid a b=if (a=0 && b!=0) then (abs(b))
                  else if (b=0 && a !=0) then (abs(a))
                  else if (a<0 || b<0) then euclid (abs(a)) (abs(b))
                  else if a=b then a
                  else if a<b then euclid a (b-a)
                  else euclid (a-b) b

(*use the euclid function I write above to simplify a fraction since we need to both the numerator and denominator needs to be divided by their gcd in order to get the simplified fraction*)
(*raise a failure if the denominator is 0;if numerator is 0 and denominator is not 0 then return (0,1) since it is the most simplified version*)
(*for negative fractions,leave the negative sign at the same places as the negative sign in the original fractions*)
(*type:(int * int)-> (int * int)*)
let frac_simplify (x,y)=if(y=0) then raise(Failure "Input denominator cannot be zero")
                        else if (x=0 && y!=0) then (0,1)
                        else if (x*y>0) then ((abs(x))/(euclid x y),(abs(y))/(euclid x y))
                        else (x/(euclid x y),y/(euclid x y))

(*this method below is to return the maximum integer in a list*)
(*raise a Failure exception if the list is empty according the wrriten up requirement in homemwork*)
(*type:int list -> int*)
let rec max : int list-> int
=fun xs->
match xs with
|[]->raise (Failure "Input list must not be empty")
|[hd]->hd
|x1::(x2::rest)->if x1>x2 then max (x1::rest) else max (x2::rest)

(*this method below is to take the first nth element of a list and return empty list if the input number is less than zero*)
(*raise an exception is the num>the length of list*)
(*int -> 'a list -> 'a list*)
let rec take num xs=
match xs with
|[]->if num >0 then raise(Failure "Invalid argument") else[]
|hd::rest->if num<=0 then [] else hd::take (num-1) rest
