(*module for stream functions we write for hw5*)
open LazeeModules
open StreamModules
module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) : (Hwk5Sig with type 'a stream='a S.t) = struct
  type 'a stream= 'a S.t
  let take=S.take
  let head=S.head
  let zip=S.zip
  let rec from (n:int) :int stream=
          S.Cons ( n,
              (S.delay) (fun () -> from (n+1) )
                )

(*the following code is to create a cube stream without using zip and map*)
  let rec cubes_from (n:int):int stream=
    S.Cons(n*n*n,(S.delay)(fun ()->cubes_from (n+1) ))

  (*since the corresponding head elements in two natural number streams
  are equal,we can just square the head element in the first stream
  and then mulitply the square result with the head element of
  the other streams*)
  let cubes_from_zip (n:int):int stream=
    zip (fun hd1 hd2-> hd1*hd1*hd2) (from n) (from n)

  (*using map function to S.Construct a cube stream below*)
  let cubes_from_map (n:int):int stream=(S.map) (fun n->n*n*n) (from n)

  (*the drop function below is to drop the first n element in the stream*)
  let rec drop (n:int) (s:'a stream):'a stream=
    match n, s with
    | 0, _ -> s
    | _, S.Cons (v, tl) -> drop (n-1) ((S.demand) tl)

  (*the following function is to drop the first n element that
  (f hd) evaluate to false*)
  let rec drop_until (f:'a->bool) (s : 'a stream) : ('a stream) =
     match s with
     |S.Cons(v,tl) ->if f v then s else drop_until f ((S.demand) tl)

  (*foldr function below from hw5*)
  let rec foldr (f:'a->'b S.lazee->'b) (s:'a stream):'b=
    match s with
    |S.Cons(hd,tl)->
                f hd ((S.delay) (fun ()-> foldr f ((S.demand) tl)))

  (*the sum_positive_prefix function which can sum up all positive
  prefix of a stream lazily*)
  let sum_positive_prefix (ns:int stream):int=
    match ns with
    |S.Cons(hd,tl)->
      foldr (fun hd accum->if hd>0 then hd+(S.demand) accum else 0) ns

  (*a helper function sift which can remove all multiples of a given number*)
  let rec sift (tar:int) (s:int stream):int stream=
    match s with
    |S.Cons(hd,tl)->if hd mod tar=0 then sift tar ((S.demand) tl)
                  else S.Cons(hd,(S.delay)(fun ()->sift tar ((S.demand) tl)))

  (*the sieve function below which can be used to compute
  all the prime numbers*)
  let rec sieve (s:int stream):int stream=
    match s with
    |S.Cons(hd,tl)->S.Cons(hd,(S.delay)(fun ()->sieve (sift hd ((S.demand) tl))))

  let primes:int stream=sieve(from 2)
  let nats:int stream=from 1
end
