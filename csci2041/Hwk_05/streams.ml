(*The code below is from Professor Eric Van Wyk. *)
(*all the code borrowed from Eric Van Wyk is in the lazy.ml in
in public class repo*)
(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let head (s: 'a stream) : 'a = match s with
    | Cons (v, _) -> v

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1)
                                           (demand tl2) ) )

let rec from (n:int):int stream =
        Cons ( n,
            delay (fun () -> from (n+1) )
              )

let nats:int stream = from 1

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

let rec take (n:int) (s : 'a stream) : ('a list) =
      match n, s with
      | 0, _ -> []
      | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

(* The code below is from Youya Xia *)

(*the following code is to create a cube stream without using zip and map*)
let rec cubes_from (n:int):int stream=
  Cons(n*n*n,delay(fun ()->cubes_from (n+1) ))

(*since the corresponding head elements in two natural number streams
are equal,we can just square the head element in the first stream
and then mulitply the square result with the head element of
the other streams*)
let cubes_from_zip (n:int):int stream=
  zip (fun hd1 hd2-> hd1*hd1*hd2) (from n) (from n)

(*using map function to construct a cube stream below*)
let cubes_from_map (n:int):int stream=map (fun n->n*n*n) (from n)

(*the drop function below is to drop the first n element in the stream*)
let rec drop (n:int) (s:'a stream):'a stream=
  match n, s with
  | 0, _ -> s
  | _, Cons (v, tl) -> drop (n-1) (demand tl)

(*the following function is to drop the first n element that
(f hd) evaluate to false*)
let rec drop_until (f:'a->bool) (s : 'a stream) : ('a stream) =
   match s with
   |Cons(v,tl) ->if f v then s else drop_until f (demand tl)

(*reasoning about foldr type:
assume the input stream is of an arbitrary type 'a and then since
the type of the output type can be different from the
type of the head element of the input stream,so the output is of type 'b;
so,the function f in the foldr argument must return a type 'b result
and since our function can give a result without computing the whole stream,
the function f must store the accumulative result we compute so far
in 'b lazee stream ;so the type of f is :'a ->'b lazee->'b*)

(*reasoning about foldr implementation:
we continue demand the 'b lazee to continue call the foldr
until we meet the element that does not meet our requirement so that we
must stop calling the function and return the evlaution result we get so far*)

let rec foldr (f:'a->'b lazee->'b) (s:'a stream):'b=
  match s with
  |Cons(hd,tl)->
              f hd (delay (fun ()-> foldr f (demand tl)))

(*the and_fold function which can lazily evaluate the boolean stream*)
let and_fold (s:bool stream):bool=
  match s with
  |Cons(hd,tl)->
    foldr (fun hd accum->if hd then hd && demand accum else false) s

(*the sum_positive_prefix function which can sum up all positive
prefix of a stream lazily*)
let sum_positive_prefix (ns:int stream):int=
  match ns with
  |Cons(hd,tl)->foldr (fun hd accum->if hd>0 then hd+demand accum else 0) ns

(*a helper function sift which can remove all multiples of a given number*)
let rec sift (tar:int) (s:int stream):int stream=
  match s with
  |Cons(hd,tl)->if hd mod tar=0 then sift tar (demand tl)
                else Cons(hd,delay(fun ()->sift tar (demand tl)))

(*the sieve function below which can be used to compute
all the prime numbers*)
let rec sieve (s:int stream):int stream=
  match s with
  |Cons(hd,tl)->Cons(hd,delay(fun ()->sieve (sift hd (demand tl))))

(*testing stream below given by the hw*)
let ns : int stream = zip ( - ) (from 1000) (cubes_from 1)

let are_positive (ns:int stream):bool stream = map (fun n -> n > 0) ns

let ns_positive : bool stream = are_positive ns
