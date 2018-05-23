open Ordered
module type RedBlackSetSig=sig
  type elem
  type color = R | B
  type t = E | T of color * t * elem * t
  val empty:t
  val insert:elem->t->t
  val member:elem->t->bool
  val isRedBlackTree: t -> bool
  val findMin:t->elem
end

module RedBlackTree (O:OrderedSig):(RedBlackSetSig with type elem=O.t)=struct
  type elem=O.t
  type color = R | B
  type t = E | T of color * t * elem * t
  (*a function to return the empty tree*)
  let empty:t=E
  (*this is a function which is aimed to determine
  if the elem is in the tree*)
  let rec member (elem:elem) (tree:t):bool=
    match elem,tree with
    |x,E->false
    |x, T(_,a,y,b)->
      if O.lt x y
      then member x a
      else if O.eq x y
      then true
      else  member x b
  (*this is a helper function to balance a tree to help to make it
  a redblack tree*)
  let balance (c:color) (t1:t) (v:elem) (t2:t):t=
      match c,t1,v,t2 with
      |B,T(R,T(R,a,x,b),y,c),z,d
      |B,T(R,a,x,T(R,b,y,c)),z,d
      |B,a,x,T(R,T(R,b,y,c),z,d)
      |B,a,x,T(R,b,y,T(R,c,z,d))
      ->T(R,T(B,a,x,b),y,T(B,c,z,d))
      |c,t1,v,t2->T(c,t1,v,t2)

  (*this is a function which is to insert an element into the tree*)
  let insert (x:elem) (s:t)=
      let rec ins (t:t):t=match t with
      |E->T(R,E,x,E)
      |T(c,a,y,b) as s->
          if O.lt x y
          then balance c (ins a) y b
          else
          if O.eq x y then s
          else balance c a y (ins b)
        in match ins s with
        |T(_,a,y,b)->T(B,a,y,b)
        |E->raise(Failure"invalid_argument")

  let root (tree:t):elem=
    match tree with
    |E->raise(Failure"no rank")
    |T(c,tl,v,tr)->v

  (*the two functions below are to compare the two elem and return the
  max and min elem respectively*)
  let tmax (elem1:elem) (elem2:elem):elem
    =if O.lt elem1 elem2 then elem2 else elem1

  let tmin (elem1:elem) (elem2:elem):elem
    =if O.lt elem1 elem2 then elem1 else elem2

  (*find the max value in the whole tree*)
  let rec findMax (tree:t):elem=
    match tree with
    |E->raise(Failure"no max value")
    |T(c,E,v,E)->v
    |T(c,tl,v,E)->
      tmax v (findMax tl)
    |T(c,E,v,tr)->
      tmax v (findMax tr)
    |T(c,tl,v,tr)->
      let rres=tmax v (findMax tr)
      in
      let lres=tmax v (findMax tl)
      in
      tmax rres lres
  (*find the min value in the whole tree*)
  let rec findMin (tree:t):elem=
      match tree with
      |E->raise(Failure"no min value")
      |T(c,E,v,E)->v
      |T(c,tl,v,E)->
        tmin v (findMin tl)
      |T(c,E,v,tr)->
        tmin v (findMin tr)
      |T(c,tl,v,tr)->
          let rres=tmin v (findMin tr)
          in
          let lres=tmin v (findMin tl)
          in
          tmin rres lres

  (*this is a function to check if the tree is a binary search tree
  using the property that the right element must be greater than
  the root of the tree and the element in the left
  should be less than the root*)
  let rec isBinaryTree (tree:t):bool=
    match tree with
    |E->true
    |T(c,E,v,E)->true
    |T(c,t1,v,E)->O.lt(findMax t1) v
    |T(c,E,v,t2)->not(O.leq (findMin t2) v)
    |T(c,t1,v,t2)->O.lt(findMax t1) v
                  && (not(O.leq (findMin t2) v))

  (*extra credit:I do not generate all path in order to check that all
  path have the same number of black nodes.In fact,I just check that
  the left and right branch has the same number of black nodes recurisvely
  by returning the number of black nodes(i.e black height)in the path.
  if the left branch and the right branch do not
  have the same number of black nodes or the left branch or the right branch
  is not a redblack subtree,then I just return 0 to indicate
  it is not a redblack tree.Else, I just determine if the parent of the left
  and right branch is black or not,if it is black,then I just
  add 1 to the black height of the tree,if it is not,then I do not
  add any value to the black height of the tree.Finally,if the final return
  value is 0,this implies that the tree is not a redblack tree,so
  I just return false,else return true to indicate that all paths have
  the same number of black nodes*)
  let checkPath (tree:t):bool=
    let rec blackNum (tree:t):int =
      match tree with
      |E->1
      |T(c,t1,v,t2)->
        let temp1=blackNum t1
        in
        let temp2=blackNum t2
        in
        if ((temp1<> temp2) || temp1=0 || temp2=0)
        then 0
        else if c=B then temp1+1
        else temp1
      in
      not (blackNum tree=0)

  (*the redBlack tree must satisfies that
  (1):every empty node is black
  (2):all children of red node are black
  (3):every path from root to a leaf must has the same
  number of black nodes
  (4):the root is colored black
  (5): it must be a binary search tree with no duplicate element based
  on the insert function of redblack tree*)
  (*this is a function which is to check if the tree is a redBlack
  tree based on the invariants stated above*)
  let isRedBlackTree (tree:t):bool =
    match tree with
    |E->true
    |T(c,t1,v,t2)->
      let rec helper tree=
        match tree with
        |E->true
        |T(c,E,v,E)->true
        |T(R,T(R,a,x,b),y,tr)->false
        |T(R,tl,x,T(R,b,y,c))->false
        |T(c,tl,v,tr)->helper tl && helper tr
      in (c=B) && helper tree && checkPath tree && isBinaryTree tree
end
(*some test cases I write below*)
module RBTI = RedBlackTree (Int)
let h1 = RBTI.empty
let h2 = RBTI.insert 20 h1
let h3 = RBTI.insert 30 h2
let h4 = RBTI.insert 10 h3
let h5 = RBTI.insert 40 h4
let l1=RBTI.isRedBlackTree h1
let l2=RBTI.isRedBlackTree h2
let l3=RBTI.isRedBlackTree h3
let l4=RBTI.isRedBlackTree h4
let l5=RBTI.isRedBlackTree h5
let l6=
  RBTI.isRedBlackTree (RBTI.T (RBTI.B, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
 2, RBTI.T (RBTI.R, RBTI.T (RBTI.R, RBTI.E, 3, RBTI.E), 4,
 RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
let l7=
  RBTI.isRedBlackTree (RBTI.T (RBTI.B, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
  2, RBTI.T (RBTI.R, RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E), 4,
  RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
let l8=
    RBTI.isRedBlackTree(RBTI.T (RBTI.B, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
    2, RBTI.T (RBTI.R, RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E), 4,
    RBTI.T (RBTI.B, RBTI.E, 2, RBTI.E))))
let l9=
    RBTI.isRedBlackTree(RBTI.T (RBTI.B, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
    2, RBTI.T (RBTI.R, RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E), 4,
    RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))
