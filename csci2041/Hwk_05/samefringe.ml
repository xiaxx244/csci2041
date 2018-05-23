type 'a bintree
  = Lf of 'a
  | Nd of 'a bintree * 'a bintree

let t1 = Nd(Nd(Lf(1), Lf(2)), Nd(Lf(3), Lf(4)))
and t2 = Nd(Lf(1), Nd(Lf(2), Nd(Lf(3), Lf(4))))
and t3 = Nd(Nd(Nd(Lf(1), Lf(2)), Lf(3)), Lf(4))
and t4 = Nd(Lf(4), Nd(Lf(3), Nd(Lf(2), Lf(1))))
and t5 = Nd(Nd(Nd(Lf(4), Lf(3)), Lf(2)), Lf(1))

(* Trees t1, t2, and t3 have the same fringe. *)
(* Trees t4 and t5 have the same fringe. *)
type 'a lazee = 'a hidden ref
and 'a hidden
	= Value of 'a
	| Thunk of (unit -> 'a);;

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")


type 'a lazy_list
  = Cons of 'a * 'a lazy_list lazee
  | Nil
(*in this append_lazy function,we just append the head of the first list to the
second list and delay the appending process*)
let rec append_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : 'a lazy_list =
  match l1 with
  |Nil->l2
  |Cons(hd,tl)->Cons(hd,delay(fun ()->append_lazy (demand tl) l2))

(*in this function,we just need to call equal_list_lazy only
when the two head elements in the two lazy list are the same,
so that we do not need to build the calling stack as large as the strict
version*)
let rec equal_list_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : bool =
  match l1,l2 with
  |Nil,Nil->true
  |Cons(hd1,tl1),Cons(hd2,tl2)->if hd1=hd2
    then equal_list_lazy (demand tl1) (demand tl2) else false
  |_,_->false

(*flatten a tree lazily below*)
let rec flatten_lazy (t: 'a bintree) : 'a lazy_list =
  match t with
|Lf a->Cons(a,ref (Value Nil))
|Nd(left,right)->append_lazy (flatten_lazy left) (flatten_lazy right)

(*this function below is to use all of the above defined function to compare
two bintrees; and I handle some special cases in the above three cases
since all of the first three cases are trivial and we do not need to call the
above function to determine whether or not the two trees have the same leaves;
in the next two cases,I handle the cases where both of the two Nds
have a leaf in the same direction so that we just need to comapre the two
leaves and then decide if we need to call the above functions;
the last case is a general case where we just need to call the above functions
to determine the boolean value*)
let eqleaves_lazy (t1: 'a bintree) (t2: 'a bintree) : bool =
match t1,t2 with
|Lf a,Lf b->a=b
|(Lf a,Nd _)->false
|(Nd _,Lf b)->false
|Nd(Lf a,right1),Nd(Lf b,right2)->if a=b
              then  equal_list_lazy (flatten_lazy right1) (flatten_lazy right2)
              else false
|Nd(left1,Lf a),Nd(left2,Lf b)->if a=b
              then  equal_list_lazy (flatten_lazy left1) (flatten_lazy left2)
              else false
|_,_->equal_list_lazy (flatten_lazy t1) (flatten_lazy t2)


(*space complexity comparision:
the strict version needs O(n) space since we need to flatten the
whole two trees, so that when we call the equal_list function,
we need to call it n times in order to compare the two tree list,in this case,
the stack we build takes up O(n) space;
However, in the lazy version,since the the list we get from flatten the tree
is a lazy_list,we indeed only have one actual element in the lazy_list and we
only need to call the append_lazy when the current two elements we look
at are the same,if not ,then we just return false;
this means we only flatten part of the tree and call the eqleaves_lazy only
when the current two elements we look at are the same;
Since in the lazy version,we only indeed flatten part of our tree and the
calling stack frame we build is smaller than the calling stack for the eager
version,so the lazy version use less space than eager version*)
