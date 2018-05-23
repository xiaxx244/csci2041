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
  | Let of string * expr * expr
  | Id of string
  | App of expr * expr
  |Multilambda of string list* expr
  | Lambda of string * expr
  | LetRec of string * expr * expr
  | If of expr * expr * expr

(*add Ref value to help to evaluate recusive functions*)
and value
  = Int of int
  | Bool of bool
  |Ref of value ref
  | Closure of string * expr * environment

and environment = (string * value) list

(*serialize the expressions*)
(*type:expr -> string*)
let rec serialize expr=
  match expr with
  |Id n->"Id \""^n^"\""
  |Val (Int a)->"Val (Int "^string_of_int a^")"
  |Val (Bool b)->"Val (Bool "^string_of_bool b^")"
  |Add(expr1,expr2)->"Add ("^serialize expr1^", "^serialize expr2^")"
  |Sub(expr1,expr2)->"Sub ("^serialize expr1^", "^serialize expr2^")"
  |Mul(expr1,expr2)->"Mul ("^serialize expr1^", "^serialize expr2^")"
  |Div(expr1,expr2)->"Div ("^serialize expr1^", "^serialize expr2^")"
  |Lt(expr1,expr2)->"Lt ("^serialize expr1^", "^serialize expr2^")"
  |Eq(expr1,expr2)->"Eq ("^serialize expr1^", "^serialize expr2^")"
  |And(expr1,expr2)->"And ("^serialize expr1^", "^serialize expr2^")"
  |Not expr1->"Not ("^serialize expr1^")"
  |Let(str1,expr1,expr2)->
      "Let "^"("^"\""^str1^"\""^", "^serialize expr1^", " ^serialize expr2^")"
  |Lambda(str1,expr1)->"Lambda ("^"\""^str1^"\""^", "^serialize expr1^")"
  |LetRec(str1,expr1,expr2)->
    "LetRec "^"("^"\""^str1^"\""^", "^serialize expr1^", " ^serialize expr2^")"
  |If(expr1,expr2,expr3)->
    "If ("^serialize expr1^", "^serialize expr2^", "^serialize expr3^")"
  |App(e1,e2)->"App ("^serialize e1^", "^serialize e2^")"
  |_->raise(Failure "Will only serialize integer and Boolean values")

(*unparse the expressions to formulate normal ocaml expressions*)
(*type:expr->string*)
  let rec unparse expr=
    match expr with
    |Id n->n
    |Val a->(match a with
            |Int b->string_of_int b
            |Bool b->string_of_bool b
            |_->raise(Failure "Will only unparse integer and Boolean values"))
    |Add(e1,e2)->"("^unparse e1^" + "^unparse e2^")"
    |Mul(e1,e2)->"("^unparse e1^" * "^unparse e2^")"
    |Sub(e1,e2)->"("^unparse e1^" - "^unparse e2^")"
    |Div(e1,e2)->"("^unparse e1^" / "^unparse e2^")"
    |Lt(e1,e2)->"("^unparse e1^" < "^unparse e2^")"
    |Eq(e1,e2)->"("^unparse e1^" = "^unparse e2^")"
    |App(e1,e2)->"("^unparse e1^" "^unparse e2^")"
    |Lambda(str,e)->"(fun "^str^" -> "^unparse e^")"
    |LetRec(str,e1,e2)->"(let rec "^str^ " = "^unparse e1^" in "^unparse e2^")"
    |If(e1,e2,e3)->
                "(if "^unparse e1^" then "^unparse e2^" else "^unparse e3^")"
    |And(e1,e2)->"("^unparse e1 ^" && "^unparse e2^")"
    |Not e->"(not "^unparse e^")"
    |Let(str,e1,e2)->"("^"let "^str^" = "^unparse e1^" in " ^unparse e2^")"

  (*find freevars in expreesions and return a list*)
  (*type:expr->string list*)
  let rec freevars (e: expr) : string list =
    match e with
    | Val _ -> []
    |Add (e1, e2)|Sub(e1,e2)|Mul(e1,e2)|Div(e1,e2)->freevars e1 @ freevars e2
    |Lt(e1,e2)|Eq(e1,e2)|And(e1,e2)|App(e1,e2)-> freevars e1 @ freevars e2
    |Let (n, dexpr, body)->
       freevars dexpr @ (List.filter (fun n' -> n <> n') (freevars body))
    |Lambda(n,body) ->(List.filter (fun n' -> n <> n') (freevars body))
    |If(e1,e2,e3)->freevars e1 @ freevars e2@freevars e3
    |LetRec(n,dexpr,body)->
      List.filter (fun n' -> n <> n') ((freevars dexpr)@(freevars body))
    |Not e->freevars e
    | Id n -> [n]

  (*look up the value of a string identifier in environment*)
  (*type:string-> environment-> value*)
  let rec lookup (n:string) (env: environment) : value =
    match env with
    | [] -> raise (Failure ("Identifier " ^ n ^ " not in scope"))
    | (n',v) :: rest when n = n' -> (match v with
        | Ref v -> !v
        | v -> v
        )
    | _ :: rest -> lookup n rest

let rec clear ls1 ls2=
  match ls2 with
  |[]->[]
  |(x,y)::rest->if List.mem x ls1 then (x,y)::clear ls1 rest else clear ls1 rest

(*evaluate expressions*)
(*type:environment -> expr -> value*)
  let rec eval (env: environment) (e: expr): value =
    match e with
    | Val v -> v

    | Add (e1, e2) ->
      ( match eval env e1, eval env e2 with
        | Int i1, Int i2 -> Int (i1 + i2)
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
    | Eq (e1, e2) ->
       ( match eval env e1, eval env e2 with
         | Int i1, Int i2 -> Bool (i1 = i2)
         | Bool b1, Bool b2 -> Bool (b1 = b2)
         | _, _ -> raise (Failure "incompatible values on Eq")
       )
    | Not e1 ->
       ( match eval env e1 with
         | Bool b -> Bool (not b)
         | _ -> raise (Failure "incompatible value on Not")
       )

    | Let (n, bexpr, body) ->
       let bexpr_v = eval env bexpr in
       eval ((n,bexpr_v)::env) body
    (*since App is a function application,raise failure if the evalution of
    first arugment is not a closure*)
    |App(e1,e2)->(match eval env e1, eval env e2 with
                  |Closure (str,e,env),vals-> eval ((str,vals)::env) e
                  |_ ->raise(Failure "incompatible value on App"))
    |Id n -> lookup n env
    |Multilambda(ls,e)->(match ls with
                        |[]->raise(Failure "dkfajl")
                        |a::[]->Closure(a,Val (eval env e),env )
                        |hd::rest->let temp=Lambda(hd,Val (eval env (Multilambda (rest,e))))
                                    in
                                    eval env temp
                        )
    |Lambda(str,expr)->
                        Closure(str,expr,env)
    |If(e1,e2,e3)->(match eval env e1 with
                    |Bool i->if i then eval env e2 else eval env e3
                    |_->raise (Failure "incompatible value on If"))
    |LetRec(str1,e1,e2)-> (match eval env e1 with
                          | Closure(_,_,_)->let recRef = ref (Int 999) in
                                  let c = eval ((str1, Ref recRef)::env) e1 in
                                  let () = recRef := c in
                                  eval ((str1,c)::env) e2
                          |_->raise (Failure "incompatible with type LetRec"))


  (* A helper function to start evaluation with the empty environment. *)
  let evaluate e = eval [] e

(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = evaluate e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = evaluate e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y",
              Val (Int 5),
              Let ("x",
                   Add (Id "y", Val (Int 5)),
                   Add (Id "x", Id "y")
                  )
             )

let () =
  assert (serialize e1 = "Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))");
  assert (serialize e6 =
            "Let (\"y\", Val (Int 5), Let (\"x\", " ^
             "Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))")


(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true)


(* increment *)
let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )
let inc' = App (add, Val (Int 1))

(* The add2 closure *)
let add2app =
  Let ("add2",
       Let ("two", Val (Int 2), Lambda ("x", Add (Id "x", Id "two"))),
       App (Id "add2", Val (Int 4)))

let () =
  assert (evaluate (App (inc, Val (Int 4))) = Int 5);
  assert (evaluate (Add (Val (Int 2), Val (Int 3))) = Int 5);
  assert (evaluate (App (inc', Val (Int 4))) = Int 5);
  assert (evaluate add2app = Int 6)


(* sumToN *)
let sumToN : expr =
    LetRec ("sumToN",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n",
                             App (Id "sumToN",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(* factorial *)
let fact : expr =
    LetRec ("fact",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 1),
                        Mul (Id "n",
                             App (Id "fact",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "fact"
           )

(* Assert expressions to test our functions. *)
let () =
  assert (evaluate (App (sumToN, Val (Int 4))) = Int 10);
  assert (evaluate (App (sumToN, Val (Int 10))) = Int 55);
  assert (evaluate (App (sumToN, Val (Int 100))) = Int 5050);
  assert (evaluate (App (fact, Val (Int 0))) = Int 1);
  assert (evaluate (App (fact, Val (Int 1))) = Int 1);
  assert (evaluate (App (fact, Val (Int 2))) = Int 2);
  assert (evaluate (App (fact, Val (Int 4))) = Int 24)



(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
  print_endline ("Success! All tests passed.")
