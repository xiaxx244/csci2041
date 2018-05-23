(*this function below is to return all even integers in a list*)
(*type:int list -> int list*)
let all_evens xs=List.filter (fun x->x mod 2=0) xs

(*this function below is to increment all elements in a list*)
(*type:int list -> int list*)
let increment_all xs=List.map(fun x ->x+1) xs

(*this function below is to return the max integer in a list*)
(*using List.fold_left by defining a function to compare two integers and return the larger one*)
(*type:int list -> int*)
let max_fold : int list-> int
=fun xs->
match xs with
|[]->raise (Failure "Input list must not be empty")
|hd ::rest->List.fold_left (fun x y->if x >y then x else y) hd (hd::rest)

(*this function is to use the List.fold_left function to return the sum and product of all element in a list as a tuple*)
(*return (0,1) if the list is empty*)
(*int list -> int * int*)
let sum_prod xs=List.fold_left (fun (l1,l2) x->(l1+x,l2*x)) (0,1) xs

(*this function below is to use List.fold_right to split the list based on the function f in the arguments*)
(*define a helper function to return a two-dimensional list*)
(*if f hd,then just append an empty list to the accumulator,if not then accesss the first element x1 in the accumulator and append hd to x1*)
(*return [[]] if the list is empty*)
(*type:('a -> bool) -> 'a list -> 'a list list*)
let split f xs=
  let split_helper (current_group,all_group) hd=
    if f hd then ([],(List.rev current_group)::all_group)
    else (hd::current_group,all_group)
  in
  let (group1,accum)=List.fold_left split_helper ([],[]) xs
  in List.rev ((List.rev group1):: accum)
