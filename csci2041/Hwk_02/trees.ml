type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree

let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))

(*this function below is to return the size of a tree*)
(*type:'a tree -> int*)
let t_size tree=
let rec t_size1 tree acc=
  match tree with
  |Leaf a->(a::acc)
  |Fork (a,left,right)->let temp1=t_size1 left (a::acc)
                        in
                        let temp2=t_size1 right (acc)
                        in temp1@temp2
  in
  match tree with
  |Leaf a->[]
  |_->(t_size1 tree [])


(*this function below is to return the sum in an int tree*)
(*int tree -> int*)
let rec t_sum tree=
  match tree with
  |Leaf a->a
  |Fork(a,left,right)->a+t_sum left +t_sum right

(*this function below is to return the sum of the length of a string tree*)
(*type:string tree -> int*)
let rec t_charcount tree=
  match tree with
  |Leaf a->String.length a
  |Fork(a,left,right)->
   String.length a+t_charcount left +t_charcount right

(*this function below is to return the concatation of the strings in a tree*)
(*type:string tree -> string*)
let rec t_concat tree=
  match tree with
  |Leaf a->a
  |Fork (a,left,right)->a^(t_concat left)^(t_concat right)

let t5 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

(*this function below is to return the size of an option tree*)
(*type:'a option tree -> int*)
let rec t_opt_size tree=
  match tree with
  |Leaf None->0
  |Leaf (Some a)->1
  |Fork(a,left,right)->match a with
                    |Some a->1+t_opt_size left +t_opt_size right
                    |None->t_opt_size left +t_opt_size right

(*this function below is to return the sum of an int option tree*)
(*type:int option tree -> int*)
let rec t_opt_sum tree=
  match tree with
  |Leaf None->0
  |Leaf (Some a)->a
  |Fork (a,left,right)->match a with
                        |Some a->a+t_opt_sum left +t_opt_sum right
                        |None->t_opt_sum left +t_opt_sum right

(*this function below is to return count of length of a string option tree*)
(*type:string option tree -> int*)
let rec t_opt_charcount tree=
  match tree with
  |Leaf None->0
  |Leaf (Some a)->String.length a
  |Fork(a,left,right)->match a with
                      |Some a->String.length a+(t_opt_charcount left)+(t_opt_charcount right)
                      |None->(t_opt_charcount left)+(t_opt_charcount right)

(*this function concatation of strings in a string tree*)
(*type:string option tree -> string*)
let rec t_opt_concat tree=
  match tree with
  |Leaf None->""
  |Leaf (Some a)->a
  |Fork(a,left,right)->match a with
                      |Some a->a^(t_opt_concat left)^(t_opt_concat right)
                      |None->t_opt_concat left^t_opt_concat right



let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b =
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

(*the following eight functions are to use tfold to implement the previous functions*)
(*they are of the same types as previous functions*)
let tf_size tree=
  let l x=1
  in
  let f x left right=(l x)+left+right
  in tfold l f tree

let tf_sum tree=
  let l x=x
  in
  let f x left right=(l x)+left+right
  in tfold l f tree

let tf_concat tree=
  let l x=x
  in
  let f x left right=(l x)^left^right
  in tfold l f tree

let tf_charcount tree=
  let l a=String.length a
  in
  let f a left right=(l a)+left+right
  in tfold l f tree

let tf_opt_size (tree:'a option tree):int=
  let l a=match a with
          |None->0
          |Some a->1
  in
  let f x left right=(l x)+left+right
  in tfold l f tree

let tf_opt_sum (tree :int option tree):int=
  let l a=match a with
          |None->0
          |Some a->a
  in
  let f x left right=(l x)+left+right
  in tfold l f tree

  let tf_opt_charcount (tree :string option tree):int=
    let l a=match a with
            |None->0
            |Some a->String.length a
    in
    let f x left right=(l x)+left+right
    in tfold l f tree

    let tf_opt_concat (tree :string option tree):string=
      let l a=match a with
              |None->""
              |Some a->a
      in
      let f x left right=(l x)^left^right
      in tfold l f tree

type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree

let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

(*this function below is to insert an element to a int ordered tree*)
(*type:('a -> 'a -> int) -> 'a -> 'a btree -> 'a btree*)
let rec bt_insert_by (f:'a->'a->int) (x:'a) (tree:'a btree):'a btree=
  match tree with
  |Empty->Node(Empty,x,Empty)
  |Node(left,a,right)->if (f x a)=1 then Node(left,a,(bt_insert_by f x right))
                       else Node((bt_insert_by f x left),a,right)

(*this function below is to determine if an element exists in a btree*)
(*type:('a -> 'b -> bool) -> 'b -> 'a btree -> bool*)
let rec bt_elem_by f x tree=
  match tree with
  |Empty->false
  |Node(left,a,right)->
                    (f a x)||(bt_elem_by f x left)||(bt_elem_by f x right)

(*this function below is to return a list of all values in a btree*)
(*type:'a btree -> 'a list*)
let rec bt_to_list tree=
  match tree with
  |Empty->[]
  |Node(left,a,right)->(bt_to_list left)@[a]@(bt_to_list right)

(*this function below is to fold a btree*)
(*type:'b -> ('b -> 'a -> 'b -> 'b) -> 'a btree -> 'b*)
let rec btfold (base:'b) (f:'b->'a->'b->'b) (tree:'a btree):'b=
  match tree with
  |Empty->base
  |Node (left,a,right)->let temp1=btfold base f left
                        in
                        let temp2=btfold base f right
                        in
                        f temp1 a temp2
(*the following two functions are to use btfold to implement the previous two functions*)
(*they are of the same type as previous two function*)
let btf_to_list tree=
  match tree with
  |Empty->[]
  |Node (left,a,right)->
                      btfold [] (fun left a right->left@[a]@right) tree

let btf_elem_by f x tree=
  match tree with
  |Empty->false
  |Node (left,a,right)->
                      btfold false (fun left a right->left ||(f a x)||right) tree


(*using btfold to write btf_insert_by is difficult,specific reasoning as follows*)
(*since if the element is greater than the root of the tree*)
(*it is diffcult to insert the element into the right place*)
(*because we can not tell if the element is greater than the rest of the right tree*)
(*same reasoning if the inserted element is less than the root of the tree*)
(*so we have to traverse the whole tree to find the right place to insert the target element*)
(*However, based on the impelementation of btfold*)
(*since btfold will traverse the right and left subtrees at the same time*)
(*so it is difficult to mark the right place that the target element should be inserted*)
(*also,we cannot get back to the right place after the traversal of the tree*)
(*therefore,using the btfold to implement btf_insert_by is difficult*)
