open LazeeModules
module type StreamSig_v2 = sig
  type 'a lazee
  type 'a t = 'a node_t lazee
  and 'a node_t =
    | Cons of 'a * 'a t
    | Nil
  exception Empty_stream

  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a

  val head: 'a t -> 'a    (* raise Empty_stream if Nil *)
  val tail: 'a t -> 'a t  (* raise Empty_stream if Nil *)
  val take: int -> 'a t -> 'a list
  val to_list : 'a t -> 'a list
end

module Stream_v2(L:LazeeSig):(StreamSig_v2 with type 'a lazee='a L.t)=struct
  type 'a lazee='a L.t
  type 'a t = 'a node_t lazee
  and 'a node_t =
    | Cons of 'a * 'a t
    | Nil

  exception Empty_stream

  let delay=L.delay
  let demand=L.demand
  let head s=
    match (demand s) with
    |Nil->raise Empty_stream
    |Cons(hd,tl)->hd
  let tail s=
    match (demand s) with
    |Nil->raise Empty_stream
    |Cons(hd,tl)->tl

  let rec take n s=
    match n,(demand s) with
    |_,Nil->[]
    |0,_->[]
    |_,Cons(hd,tl)->hd::take (n-1) tl

  let rec to_list s=
    match (demand s) with
    |Nil->[]
    |Cons(hd,tl)->hd::to_list tl

  end
