(*work with Xingtong Zhang,Yuze Jiang,Youya Xia*)

let all_evens xs=List.filter (fun x->x mod 2=0) xs


let increment_all xs=List.map(fun x ->x+1) xs


let max_fold : int list-> int
  =fun xs->
  match xs with
  |[]->raise (Failure "Input list must not be empty")
  |hd ::rest->List.fold_left (fun x y->if x >y then x else y) hd (hd::rest)


let sum_prod xs=(List.fold_left (fun x y->x+y) 0 xs),(List.fold_left (fun x y->x*y) 1 xs)

(*I do not need to match hd and accum together*)
(*change the indenting way to make my code look nicer*)
(*since all of the three match cases do not have differeent values of hd*)
let split f xs=let split_helper hd accum=
            match accum with
            |_ when f hd->[]::accum
            |x1::list1->(hd::x1)::list1
            |[]->[[hd]]
            in List.fold_right split_helper xs [[]]
