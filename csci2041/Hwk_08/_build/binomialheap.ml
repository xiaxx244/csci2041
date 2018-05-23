open Ordered

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
  val isBinomialTree: tree -> bool
  val isBinomialHeap: t -> bool
  val findMinDirect: t -> elem

end

module BinomialHeap(O:OrderedSig):(BinomialHeapSig with type elem =O.t)=struct

  type elem=O.t

  type tree = Node of int * elem * tree list

  type t = tree list

  (*this function below is to return an empty BinomialHeap*)
  let empty:tree list=[]

  (*this function below is to determine if a binomial heap is empty*)
  let isEmpty (t:tree list):bool=
    match t with
    |[]->true
    |_->false

  (*this is a function below which is aimed to link two tree with
  equal rank r and return a new tree with rank r+1*)
  let link (Node(r1,x1,c1):tree) (Node(r2,x2,c2):tree):tree=
    if r1<>r2
    then raise(Failure "rank of the input two trees must be the same")
    else if O.leq x1 x2
    then Node(r1+1,x1,(Node(r2,x2,c2))::c1)
    else Node(r2+1,x2,(Node(r1,x1,c1))::c2)

  (*this is a function which is aimed to return the rank of a tree*)
  let rank ((Node(r,x,c)):tree):int=r

  (*this is a function which is to return the root of a tree*)
  let root ((Node (r,x,c)):tree):elem=x

  (*this is a helper function which is to insert a binomialtree
  into a binomial heap*)
  let rec insTree (t1:tree) (t2:tree list):tree list=
    match t1,t2 with
    |t,[]->[t]
    |t,hd::tl->
      if rank t < rank hd
      then t::t2
      else insTree (link t hd) tl

  (*this function is to insert an element into a Binomialheap*)
  let insert (x:elem) (t:tree list)=insTree (Node(0,x,[])) t

  (*this is a function which is aimed to merge two heaps into a
  new heap*)
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

  (*this is a function which is to remove the tree with min element in the
  heap and return the tuple which consists of the removed tree and remaining
  heap*)
  let rec removeMinTree (t:tree list):tree * tree list=
    match t with
    |[]->raise(Failure"can not remove an empty tree")
    |[hd]->(hd,[])
    |hd::tl->let (t,ts)=removeMinTree tl
              in
              if O.lt (root hd) (root t) then (hd,ts)
              else (t,hd::ts)

  (*this is a function which is to find the min element in the heap
  using remobeMinTree*)
  let rec findMin (t:tree list):elem=
    let (mintree,_)=removeMinTree t
    in
    root mintree

  (*this is a function which instead of deleting the whole tree with min
  element,the function is to only delete the min element in the heap while
  keeping the remaining children of the tree with min element
  and return a new heap without the min element*)
  let deleteMin (t:tree list):tree list=
    let (Node(_,_,t1),t2)=removeMinTree t
    in
    merge (List.rev t1) t2

  (*this is a helper functoin which is to help to check is a binomial
  tree has correct rank,i.e 2^r=the number of nodes in the tree*)
  let checkRank (tree:tree):bool=
    let rec helper (ls:tree list):int =
      match ls with
      |[]->0
      |Node(r,x,ls)::tl->1+helper ls+helper tl
    in match tree with
    |Node(r,x,[])->r=0
    |Node(r,x,ls)->2.0** (float_of_int r)=float_of_int((1+helper ls))

  (*this is a helper function which is help to check if the tree
  maintain the minheap property,i.e,the root value of every children in the
  tree list should be greater than the root of the parent*)
  let rec checkMin (Node(r,x,ls):tree):bool=
      match ls with
      |[]->true
      |Node(r1,x1,ls1)::tl->if O.leq x x1 then checkMin (Node(r,x,tl))
                            else false

  (*this is a function which is to check that not only the
  original tree maintain the minheap property,every subtree in the
  tree list also maintain the minheap property*)
  let isMinHeap (tree:tree):bool=
      let rec helper (ls:tree list):bool=
        match ls with
        |[]->true
        |Node(r,x,[])::tl->(r=0)&& (tl=[])
        |Node(r,x,Node(r1,k,v)::tl1)::tl->
            (helper (Node(r1,k,v)::tl1)) && helper tl
            && checkMin (Node(r,x,Node(r1,k,v)::tl1))
      in
      match tree with
      |Node(r,x,[])->r=0
      |Node(r,x,Node(r1,k,v)::tl)->
       helper (Node(r1,k,v)::tl)&&checkMin tree

  (*this is a helper function which is aimed to check that the original
  tree has decreasing rank,i.e the tree has children each with rank r-i*)
  let checkTreeOrder (tree:tree):bool=
    let rec helper (ls:tree list):bool=
      match ls with
      |[]->true
      |[hd]->rank hd=0
      |hd1::hd2::tl->(rank hd1=(rank hd2+1))&&helper (hd2::tl)
    in match tree with
    |Node(r,x,[])->r=0
    |Node(r,x,hd::tl)->(r=rank hd +1) && (helper (hd::tl))

  (*the BinomialTree must statisfies the invariant that
  (1):a tree of rank r+1 should be formed by linking
  together two trees of equal rank r,making one of the tree
  the leftmost of the other
  (2):the singleton node should be of rank 0
  (3):the rank of the tree should be correct(i.e.,a binomial
  tree of rank r should have exactly 2^r nodes)
  (4):the tree should satisfy minheap property(i.e the element stored in the
  parent must be less than or equal to the elments stored in its children)
  (5):the tree of rank r should consist of r children and the ith
  children should be of rank r-i*)
  (*this function below is to check that the not only
  the original tree maintain all the
  invariants stated above,every subtre also maintain the stated above
  invariants*)
  let isBinomialTree (tree:tree):bool=
    let rec helper (ls:tree list):bool=
      match ls with
      |[]->true
      |Node(r,x,[])::tl->(r=0)&& (tl=[])
      |Node(r,x,Node(r1,k,v)::tl1)::tl->
        (helper (Node(r1,k,v)::tl1)) && helper tl
        && (checkRank (Node(r,x,Node(r1,k,v)::tl1)))
        &&checkTreeOrder (Node(r,x,Node(r1,k,v)::tl1))
        && List.length (Node(r1,k,v)::tl1)=r
    in
    match tree with
    |Node(r,x,[])->r=0
    |Node(r,x,Node(r1,k,v)::tl)->
      helper (Node(r1,k,v)::tl) && (checkRank tree)
      &&checkTreeOrder tree&&(isMinHeap tree)
      &&List.length (Node(r1,k,v)::tl)=r

  (*the BinomialHeap must satisfies the invariant that
  (1):it should be consisted of Binomial Trees
  (2):for each rank k,there is at most one tree whose rank is k
  (3):it should consist of trees with inccreasing order*)
  (*this is a function below is to check if a tree list is BinomialHeap
  based on the invariants stated above*)
  let rec isBinomialHeap (heap:tree list):bool=
      match heap with
      |[]->true
      |hd1::[]->(isBinomialTree hd1)
      |hd1::hd2::tl->
        (rank hd1<rank hd2)&&(isBinomialTree hd1)&& (isBinomialHeap (hd2::tl))

  (*this is a function which is to return the min element in the tree without
  using removeMinTree or findMin instead just use the BinomialTree
  property that the root is the min element in the tree*)
  let findMinDirect (heap:tree list):elem=
      match heap with
      |[]->raise(Failure"can not find a min")
      |hd::tl->
        let rec helper (heap:tree list) (minValue:elem):elem=
          match heap with
          |[]->minValue
          |hd::tl->if O.lt (root hd) minValue then helper tl (root hd)
                    else helper tl minValue
      in helper heap (root hd)

end
(*some test cases I created below*)
module BHI = BinomialHeap(Int)

let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4

let m1 = BHI.findMin h5
let h6 = BHI.deleteMin h5

let m2 = BHI.findMin h6
let l1=BHI.isBinomialHeap h1
let l2=BHI.isBinomialHeap h2
let l3=BHI.isBinomialHeap h3
let l4=BHI.isBinomialHeap h4
let l5=BHI.isBinomialHeap h5
let l6=BHI.isBinomialHeap h6
let tree1=
  BHI.Node(2,14,[
    BHI.Node(1,15,[BHI.Node(0,53,[])]);BHI.Node(0,24,[])])
let tree2=
      BHI.Node(3,12,(tree1::[
        BHI.Node(1,15,[BHI.Node(0,23,[])]);BHI.Node(0,14,[])]))
let l7=BHI.isBinomialTree tree2
let tree3=
  BHI.Node(2,14,[
    BHI.Node(1,15,[BHI.Node(0,53,[])]);BHI.Node(0,80,[])])
let tree4=
      BHI.Node(3,12,(tree3::[
        BHI.Node(1,15,[BHI.Node(0,100,[])]);BHI.Node(0,9,[])]))
let l8=BHI.isBinomialTree tree4
