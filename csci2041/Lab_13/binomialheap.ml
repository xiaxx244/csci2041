module type OrderedSig=sig
  type t
  val eq:t->t->bool
  val lt:t->t->bool
  val leq:t->t->bool
end

module Int:(OrderedSig with type t=int)=struct
 type t=int
 let eq a b=(a=b)
 let lt a b=a<b
 let leq a b=a<=b
end

module type BinomialHeapSig = sig
  type elem
  type tree = Node of int * elem * tree list
  type t = tree list
  val empty:tree list
  val isEmpty:tree list->bool
  val insert:elem->tree list->tree list
  val merge:tree list->tree list->tree list
  val findMin: tree list->elem
  val deleteMin:tree list->tree list
end

module BinomialHeap (O:OrderedSig):(BinomialHeapSig with type elem =O.t)=struct

  type elem=O.t

  type tree = Node of int * elem * tree list

  type t = tree list

  let empty:tree list=[]

  let isEmpty (t:tree list):bool=
    match t with
    |[]->true
    |_->false

  let link (Node(r1,x1,c1):tree) (Node(r2,x2,c2):tree):tree=
    if r1<>r2
    then raise(Failure "rank of the input two trees must be the same")
    else if O.leq x1 x2
    then Node(r1+1,x1,(Node(r2,x2,c2))::c1)
    else Node(r2+1,x2,(Node(r1,x1,c1))::c2)

  let rank ((Node(r,x,c)):tree):int=r

  let root ((Node (r,x,c)):tree):elem=x

  let rec insTree (t1:tree) (t2:tree list):tree list=
    match t1,t2 with
    |t,[]->[t]
    |t,hd::tl->
      if rank t < rank hd
      then t::t2
      else insTree (link t hd) tl

  let insert (x:elem) (t:tree list)=insTree (Node(0,x,[])) t

  let rec merge (t1:tree list) (t2:tree list):tree list=
    match t1,t2 with
    |t1',[]->t1'
    |[],t2'->t2'
    |hd1::tl1,hd2::tl2->
      if rank hd1<rank hd2
      then hd1::merge tl1 t2
      else if rank hd1>rank hd2
      then hd2::merge t1 tl2
      else insTree (link hd1 hd2) (merge tl1 tl2)

  let rec removeMinTree (t:tree list):tree * tree list=
    match t with
    |[hd]->(hd,[])
    |hd::tl->let (t,ts)=removeMinTree tl
              in
              if O.lt (root hd) (root t) then (hd,ts)
              else (t,hd::ts)

  let rec findMin (t:tree list):elem=
    let (mintree,_)=removeMinTree t
    in
    root mintree

  let deleteMin (t:tree list):tree list=
    let (Node(_,_,t1),t2)=removeMinTree t
    in
    merge (List.rev t1) t2
end

module BHI = BinomialHeap(Int)
let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4

let m1 = BHI.findMin h5

let h6 = BHI.deleteMin h5

let m2 = BHI.findMin h6
