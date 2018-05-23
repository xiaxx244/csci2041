
(*in this expr type part,I have added five types:
Match is similar to the match in ocaml which is designed to help write
List functions;\
Mod is the mathmatical mod functions;
Or is the logic expression;
Cons whiich is the same as :: in lists
Etuple is a type which is designed to store expr types*)
type expr
  = Val of value
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr
  |Mod of expr * expr
  |Or of expr * expr
  | Let of string * expr * expr
  | Id of string
  |Cons of expr * expr
  |Match of expr * expr* string* expr
  |Etuple of expr* expr
  | App of expr * expr
  | Lambda of string * expr
  | LetRec of string * expr * expr
  |If of expr * expr * expr
(*in this value part,I have added 3 types,the first type is IntList
  which is designed to store the int values,antother type is BoolList,which is
  designed to store all boolean values
  Vtuple which is designed to store the different subTypes in values
  *)
and value
  = Int of int
  | Bool of bool
  |Ref of value ref
  |List of value list
  |Vtuple of value * value
  |Closure of string * expr * environment

and environment = (string * value) list

let rec lookup (n:string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Identifier " ^ n ^ " not in scope"))
  | (n',v) :: rest when n = n' -> (match v with
      | Ref v -> !v
      |v->v
      )
  | _ :: rest -> lookup n rest


let rec eval (env: environment) (e: expr): value =
  match (e:expr) with
  | Val v -> v
  |Cons (e1,e2)->(match eval env e1,eval env e2 with
                  |hd,List ls->List (hd::ls)
                  |_->raise(Failure"incompatible type with Cons"))
  (*exclude type lists and tuples in the Add,
  sub,Mul,Div,Lt,And,Eq part since in ocaml
  the ocaml intepreter do not directly support
  all of the above operations for lists
  and we need to write recurisve functions to compute all of these values*)
  | Add (e1, e2) ->
    ( match eval env e1, eval env e2 with
      |Int i1, Int i2 -> Int (i1 + i2)
      | _, _ -> raise (Failure "incompatible values on Add")
    )

  | Sub (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Int (i1 - i2)
       | _, _ -> raise (Failure "incompatible values on Sub")
     )
  | Mul (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Int (i1 * i2)
       | _, _ -> raise (Failure "incompatible values on Mul")
     )
  |Mod (e1,e2)->(match eval env e1,eval env e2 with
                |Int v1,Int v2->Int(v1 mod v2)
                |_,_->raise(Failure "incompatible type with mod"))
  | Div (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Int (i1 / i2)
       | _, _ -> raise (Failure "incompatible values on Div")
     )

  | Lt (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Bool (i1 < i2)
       | _, _ -> raise (Failure "incompatible values on Lt")
     )
  | And (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _, _ -> raise (Failure "incompatible values on And")
     )
| Or (e1, e2) ->
        ( match eval env e1, eval env e2 with
          | Bool b1, Bool b2 -> Bool (b1 || b2)
          | _, _ -> raise (Failure "incompatible values on And")
        )
  | Eq (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Bool (i1 = i2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _, _ -> raise (Failure "incompatible values on Eq")
     )
  |Match ((Id str),e2,e3,e4)->(match eval env (Id str) with
                      |List []->eval env e2
                      |List (hd::xs)->eval ((e3,hd)::(str,List xs)::env) e4
                      |_->raise(Failure "incompatible type with match"))
  |Match _->raise(Failure "incompatible type with match")
  (*since the Etuple contains the two different expr types,therefore,
  just evaluate the results of each subtype separatly
  and put them in a Vtuple*)
  |Etuple (e1,e2)->Vtuple(eval env e1,eval env e2)
  | Not e1 ->
     ( match eval env e1 with
       | Bool b -> Bool (not b)
       | _ -> raise (Failure "incompatible value on Not")
     )
  | Let (n, bexpr, body) ->
     let bexpr_v = eval env bexpr in
     eval ((n,bexpr_v)::env) body

  |App(e1,e2)->(match eval env e1, eval env e2 with
                |Closure (str,e,env),vals-> eval ((str,vals)::env) e
                |_ ->raise(Failure "incompatible value on App"))
  |Id n -> lookup n env
  |Lambda(str,expr)->Closure(str,expr,env)
  |If(e1,e2,e3)->(match eval env e1 with
                  |Bool i->if i then eval env e2 else eval env e3
                  |_->raise (Failure "incompatible value on If"))
  |LetRec(str1,e1,e2)-> (match eval env e1 with
                        | Closure(_,_,_)->let recRef = ref (Int 999) in
                                let c = eval ((str1, Ref recRef)::env) e1 in
                                let () = recRef := c in
                                eval ((str1,c)::env) e2
                        |_->raise (Failure "incompatible with type LetRec"))

  let evaluate e = eval [] e
  (*provide sample language for my self-defined type*)
  let v1=List [Int 2;Int 3;Int 4;Int 5;Int 6]
  let v2=List [Bool true;Bool false;Bool true;Bool true]
  let v3=List [Int 3;Int 6;Int 8;Int 9;Int 7;Int 10]
  let v4=List[Int 2;Int 10;Int 15;Int 16;Int 18]
  let v5=List [Bool false;Bool false;Bool false;Bool false]
  let v6=List [Bool false;Bool false;Bool true;Bool false]
  let v7=List[Bool true;Bool true;Bool true]
  let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
  let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
  (*this is equal to ((1+2*3),(10-(1+2*3)/2)) in ocaml*)
  let e3=Etuple(e1,e2)
  let e4 = Let ("y",
                Val (Int 5),
                Let ("x",
                     Add (Id "y", Val (Int 5)),
                     Add (Id "x", Id "y")
                    )

               )
(*this is equal to
(let x = 3 < 5 in x && let x = 1 + 2 in x = 3,(10-(1+2*3)/2)) in ocaml*)
let e5=Etuple(e4,e2)


  (*this is a function that is expected to return
    the sum of all elements in the list*)
  (*in ocaml this function is equal to:let rec sumList xs=
                                        match xs with
                                        |[]->0
                                        |hd::tl->hd+sumList tl*)
  let sumList : expr =
      LetRec ("sumList",
              Lambda ("xs",
                      Match (Id "xs",
                          Val (Int 0),"hd",
                          Add (Id "hd",
                               App (Id "sumList",
                                  Id "xs")
                                   )
                              )
                         ),
              Id "sumList"
             )
(*this is a function that is expected to return
 the sum of all elements in the list*)
(*in ocaml this function is equal to:let rec multiList xs=
                                      match xs with
                                    ``|[]->1
                                      |hd::tl->hd*multiList tl*)
let multiList=LetRec ("multiList",
                Lambda ("xs",
                    Match (Id "xs",
                      Val (Int 1),"hd",
                          Mul (Id "hd",
                            App (Id "multiList",
                              Id "xs")
                      )
                    )
                ),
              Id "multiList"
            )
(*in ocaml this function is equal to:let rec evenList xs=
                                                  match xs with
                                                ``|[]->[]
                                                  |hd::tl->if hd mod 2=0
                                                  then hd::evenList tl
                                                  else evenList tl*)
let evenList=
            LetRec ("evenList",
                Lambda ("xs",
                    Match (Id "xs",
                          Val (List []),"hd",
                                  If(Eq(Mod(Id "hd",Val(Int 2)),Val(Int 0)),
                                    Cons(Id "hd",App (Id "evenList",Id "xs")),
                                    App(Id"evenList",Id "xs")))),
                                  Id "evenList"
                              )
(*in ocaml this function is equal to:let rec evenList xs=
                                      match xs with
                                      |[]->[]
                                      |hd::tl->if hd=true
                                      then hd::evenList tl
                                      else evenList tl*)
let trueList=
  LetRec ("trueList",
      Lambda ("xs",
        Match (Id "xs",
              Val (List []),"hd",
                      If(Eq(Id "hd",Val(Bool true)),
                        Cons(Id "hd",App (Id "trueList",Id "xs")),
                        App(Id "trueList",Id "xs")))),
                      Id "trueList"
                      )

(*in ocaml this function is equal to let rec andf xs=
                                             match xs with
                                            |[]->true
                                            |hd::tl->hd && andf tl*)
let andf=LetRec ("andf",
    Lambda ("xs",
      Match (Id "xs",
            Val (Bool true),"hd",
                    And(Id "hd",
                      App(Id "andf",Id "xs")))),
                    Id "andf"
                    )
(*in ocaml,this function is equal to let rec orf xs=
                                        match xs with
                                        |[]->false
                                        |hd::tl->hd || orf tl*)
let orf=LetRec ("andf",
    Lambda ("xs",
      Match (Id "xs",
            Val (Bool false),"hd",
                    Or(Id "hd",
                      App(Id "andf",Id "xs")))),
                    Id "andf"
(*let rec map f lst =
  match lst with
  | [] -> []
  | x::xs -> f x :: map f xs*)                )
let map =Lambda("f",LetRec("map",
              Lambda("xs",
              Match(Id "xs",Val(List[]),"hd",
              Cons(App(Id "f",Id "hd"),
              App(Id "map",Id "xs")))
              ),
              Id "map"))

(*let rec filter f lst =
  match lst with
  | [] -> []
  | x::xs -> if f x
             then x :: filter f xs
             else filter f xs*)
let filter =Lambda("f",LetRec("filter",
              Lambda("xs",
              Match(Id "xs",Val(List[]),"hd",
              If(Eq(App(Id "f",Id "hd"),Val(Bool true)),
              Cons(Id "hd",App(Id "filter",Id "xs")),
                App(Id "filter",Id "xs")))
              ),
              Id "filter"))
(*let rec foldr f lst base =
  match lst with
  | [] -> base
  | x::xs -> f x (foldr f xs base)*)
let foldr=Lambda("f",Lambda("base",LetRec("foldr",
              Lambda("xs",
              Match(Id "xs",Id "base","hd",
              App(App(Id "f",Id "hd"),
                App(Id "foldr",Id "xs")))
              ),
              Id "foldr")))
(* Assert expressions to test our functions. *)
let add= Lambda ("hd",
                 Add (Id "hd", Val(Int 1))
                )
let addf = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                  )

let even=Lambda("hd",If(Eq(Mod(Id "hd",Val(Int 2)),
  Val(Int 0)),Val(Bool true),Val(Bool false)))

let () =
    assert (evaluate e1 = Int 7);
    assert (evaluate e2 = Int 7);
    assert (evaluate e3 = Vtuple(Int 7,Int 7));
    assert (evaluate e4 = Int 15);
    assert (evaluate e5 = Vtuple(Int 15,Int 7));
    assert (evaluate (App (sumList, Val v1)) = Int 20);
    assert (evaluate (App (sumList, Val v3))= Int 43);
    assert (evaluate (App (sumList, Val v4)) = Int 61);
    assert (evaluate (App (multiList, Val v1)) = Int 720);
    assert (evaluate (App (multiList, Val v3)) = Int 90720);
    assert (evaluate (App (multiList, Val v4)) = Int 86400);
    assert (evaluate (App (evenList, Val v1)) = List [Int 2;Int 4;Int 6]);
    assert (evaluate (App (evenList, Val v3)) = List [Int 6;Int 8;Int 10]);
    assert (evaluate (App (evenList, Val v4)) =
      List [Int 2;Int 10;Int 16;Int 18]);
    assert (evaluate (App (trueList, Val v2)) =
      List [Bool true;Bool true;Bool true]);
    assert (evaluate (App (trueList, Val v5)) = List []);
    assert (evaluate (App (trueList, Val v6))= List [Bool true]);
    assert (evaluate (App (andf, Val v2)) = Bool false);
    assert (evaluate (App (andf, Val v7)) = Bool true);
    assert (evaluate (App (andf, Val v6))= Bool false);
    assert (evaluate (App (orf, Val v2)) = Bool true);
    assert (evaluate (App (orf, Val v5)) = Bool false);
    assert (evaluate (App (orf, Val v6))= Bool true);
    assert(evaluate(App(App(map,add),Val v1))=
      List [Int 3;Int 4;Int 5;Int 6;Int 7]);
    assert(evaluate(App(App(map,add),Val v3))=
      List[Int 4;Int 7;Int 9;Int 10;Int 8;Int 11]);
    assert(evaluate(App(App(map,add),Val v4))=
      List[Int 3;Int 11;Int 16;Int 17;Int 19]);
    assert(evaluate(App(App(filter,even),Val v1))= List [Int 2;Int 4;Int 6]);
    assert(evaluate(App(App(filter,even),Val v3))= List [Int 6;Int 8;Int 10]);
    assert(evaluate(App(App(filter,even),Val v4))=
      List [Int 2;Int 10;Int 16;Int 18]);
    assert(evaluate(App(App(App(foldr,addf),Val(Int 0)),Val v1))=Int 20);
    assert(evaluate(App(App(App(foldr,addf),Val(Int 0)),Val v3))=Int 43);
    assert(evaluate(App(App(App(foldr,addf),Val(Int 0)),Val v4))=Int 61);
    print_endline("All tests have been passed.")
