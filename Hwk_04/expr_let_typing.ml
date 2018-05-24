type value
  = Int of int
  | Bool of bool

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

type environment = (string * value) list

type typ =
  | IntType
  | BoolType

type error =
  (* An unbound name error *)
  | UnboundName of string
  | IncorrectType of expr * typ * (typ list)
  | DivisionByZero of expr


type 'a result = OK of 'a
               | Err of error list

(*reformulate serialize for the redefined expr and value*)
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

(*reformulate unparse for the redefined expr and value*)
(*expr -> string*)
let rec unparse expr=
  match expr with
  |Id n->n
  |Val a->(match a with
          |Int b->string_of_int b
          |Bool b->string_of_bool b)
  |Add(e1,e2)->"("^unparse e1^" + "^unparse e2^")"
  |Mul(e1,e2)->"("^unparse e1^" * "^unparse e2^")"
  |Sub(e1,e2)->"("^unparse e1^" - "^unparse e2^")"
  |Div(e1,e2)->"("^unparse e1^" / "^unparse e2^")"
  |Lt(e1,e2)->"("^unparse e1^" < "^unparse e2^")"
  |Eq(e1,e2)->"("^unparse e1^" = "^unparse e2^")"
  |And(e1,e2)->"("^unparse e1 ^" && "^unparse e2^")"
  |Not e->"(not "^unparse e^")"
  |Let(str,e1,e2)->"("^"let "^str^" = "^unparse e1^" in " ^unparse e2^")"

(*get the typ of a value*)
(*type:value -> typ*)
let get_type v=
    match v with
    |Int _->IntType
    |Bool _->BoolType

(*look up the result type of a string identifier in the environment*)
let rec lookup (n:string) (env: (string * 'a) list) : 'a result =
  match env with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> OK v
  | _ :: rest -> lookup n rest

(*compute the value of a correct expression and return the result type
and report the error of the wrong expression based on the requirement
in the problem*)
(*type:expr -> environment -> value result*)
let rec eval (e:expr) (env: environment) : value result =
  match e with
  |Val v -> OK v
  |Id n -> lookup n env
  |Div(e1,e2)->(match eval e1 env with
              |OK(Int v1)->(match eval e2 env with
                        |OK (Int 0)->Err [DivisionByZero e]
                        |OK (Int v2)->OK (Int (v1 / v2))
                        |OK(Bool _)->Err [IncorrectType(e2,BoolType,[IntType])]
                        |Err _->eval e2 env)
              |OK(Bool _)->Err [IncorrectType(e1,BoolType,[IntType])]
              |Err _->eval e1 env)
  |Sub(e1,e2)->(match eval e1 env with
              |OK(Int v1)->(match eval e2 env with
                        |OK (Int v2)->OK (Int (v1-v2))
                        |OK(Bool _)->Err [IncorrectType(e2,BoolType,[IntType])]
                        |Err _ ->eval e2 env)
              |OK(Bool _)->Err [IncorrectType(e1,BoolType,[IntType])]
              |Err _->eval e1 env)
  |Add(e1,e2)->(match eval e1 env with
              |OK(Int v1)->(match eval e2 env with
                        |OK (Int v2)->OK (Int (v1+v2))
                        |OK(Bool _)->Err [IncorrectType(e2,BoolType,[IntType])]
                        |Err _ ->eval e2 env)
              |OK(Bool _)->Err [IncorrectType(e1,BoolType,[IntType])]
              |Err _->eval e1 env)
  |Mul(e1,e2)->(match eval e1 env with
              |OK(Int v1)->(match eval e2 env with
                        |OK (Int v2)->OK (Int (v1*v2))
                        |OK(Bool _)->Err [IncorrectType(e2,BoolType,[IntType])]
                        |Err _ ->eval e2 env)
              |OK(Bool _)->Err [IncorrectType(e1,BoolType,[IntType])]
              |Err _->eval e1 env)
  |Lt(e1,e2)->(match eval e1 env with
              |OK(Int v1)->(match eval e2 env with
                        |OK (Int v2)->OK (Bool (v1<v2))
                        |OK(Bool _)->Err [IncorrectType(e2,BoolType,[IntType])]
                        |Err _->eval e2 env)
              |OK(Bool _)->Err [IncorrectType(e1,BoolType,[IntType])]
              |Err _->eval e1 env)
  |Eq(e1,e2)->(match eval e1 env with
              |OK(Int v1)->(match eval e2 env with
                      |OK (Int v2)->OK (Bool (v1=v2))
                      |OK(Bool _)->Err [IncorrectType(e2,BoolType,[IntType])]
                      |Err _->eval e2 env)
              |OK(Bool v1)->(match eval e2 env with
                        |OK (Bool v2)->OK (Bool (v1=v2))
                        |OK(Int _)->Err [IncorrectType(e2,IntType,[BoolType])]
                        |Err _->eval e2 env)
              |Err _->eval e1 env)
  |And(e1,e2) -> (match eval e1 env with
              |OK(Bool v1)->(match eval e2 env with
                        |OK (Bool v2)->OK (Bool (v1 && v2))
                        |OK(Int _)->Err [IncorrectType(e2,IntType,[BoolType])]
                        |Err _->eval e2 env)
              |OK(Int _)->Err [IncorrectType(e1,IntType,[BoolType])]
              |Err _->eval e1 env)
  |Not e->(match eval e env with
           |OK(Bool v)->OK (Bool (not v))
           |OK(Int _)->Err [IncorrectType(e,IntType,[BoolType])]
           |Err _->eval e env
           )
  |Let(str,bexpr,body)->(match eval bexpr env with
                        |OK bexpr_v->eval body ((str,bexpr_v)::env)
                        |_->eval bexpr env)


(* A helper function to start evaluation with the empty environment. *)
let evaluate e = eval e []


(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = eval e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = eval e2

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

(* Assert expressions to test the evaluate function. *)
let () =
  assert (evaluate e1 = OK (Int 7));
  assert (evaluate e2 = OK (Int 7));
  assert (evaluate e3 = OK (Bool true));
  assert (evaluate e4 = OK (Bool false));
  assert (evaluate e5 = OK (Bool true));
  assert (evaluate e6 = OK (Int 15));
  assert (evaluate e7 = OK (Bool true))


let er1 = Add (Val (Int 1), Mul (Val (Bool true), Val (Int 3)))
let er2 = Eq (Val (Bool true), Val (Int 3))
let er3 = Eq (e1, e4)

let er4 = Let ("y",
               Val (Int 5),
               And (Val (Bool true), Id "y")
              )

let er5 = And (Val (Bool true), Id "y")

let er6 = Let ("y",
               Val (Int 0),
               Div (Val (Int 5), Id "y")
              )

let er7 = Let ("x",
              Add (Val (Int 5), Val (Bool true)),
              Add (Id "x", Val (Int 5))
              )


(* To check the type correctness of expressions by infering their
   type, we use the following data types. *)

type context = (string * typ) list
(*this is designed to lookup the typ result
for a string identifier in the env*)
let rec lookup' (n:string) (env: (string * typ) list) : typ result =
  match env with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> (match v with
      |IntType ->OK (IntType)
      |BoolType->OK (BoolType)
      )
  | _ :: rest -> lookup' n rest

(*this function aims to return as many errors as possible in a expression
 and do not compute the value result for the correct expreession
 instead just return IntType or BoolType;
 also this function evaluates both sides of expr for operators
 in order to return as many errors as possible*)
 (*type:expr -> context -> typ result*)
let rec check (e:expr) (ctxt:context) : typ result =
  match e with
  |Val v-> OK (get_type v)
  |Id n->lookup' n ctxt
  |Lt(e1,e2)->(match check e1 ctxt, check e2 ctxt with
    |OK IntType,OK IntType->OK BoolType
    |OK IntType,OK BoolType->Err ([IncorrectType(e2,BoolType,[IntType])])
    |OK BoolType,OK IntType->Err ([IncorrectType(e1,BoolType,[IntType])])
    |OK BoolType,OK BoolType->
      Err ((IncorrectType(e1,BoolType,[IntType])::
      [IncorrectType(e2,BoolType,[IntType])]))
    |Err xs,OK BoolType->Err (xs @[(IncorrectType(e2,BoolType,[IntType]))])
    |Err xs,Err ls->Err (xs@ ls)
    |OK BoolType,Err ls->Err ((IncorrectType(e1,BoolType,[IntType])::ls))
    |OK IntType,Err ls->Err ls
    |Err ls,OK IntType->Err ls)
  |Add(e1,e2)|Sub(e1,e2)|Div(e1,e2)|Mul(e1,e2)->
              (match check e1 ctxt, check e2 ctxt with
              |OK IntType,OK IntType->OK IntType
              |OK IntType,OK BoolType->
                Err ([IncorrectType(e2,BoolType,[IntType])])
              |OK BoolType,OK IntType->
                Err ([IncorrectType(e1,BoolType,[IntType])])
              |OK BoolType,OK BoolType->
                Err ((IncorrectType(e1,BoolType,[IntType])::
                [IncorrectType(e2,BoolType,[IntType])]))
              |Err xs,OK BoolType->
                Err (xs@[(IncorrectType(e2,BoolType,[IntType]))])
              |Err xs,Err ls->Err (xs@ ls)
              |OK BoolType,Err ls->
                Err ((IncorrectType(e1,BoolType,[IntType])::ls))
              |OK IntType,Err ls->Err ls
              |Err ls,OK IntType->Err ls)
  |Eq (e1,e2)->(match check e1 ctxt, check e2 ctxt with
              |OK IntType,OK IntType->OK BoolType
              |OK BoolType,OK BoolType->OK BoolType
              |OK IntType,OK BoolType->
                Err ([IncorrectType(e2,BoolType,[IntType])])
              |OK BoolType,OK IntType->
                Err ([IncorrectType(e2,IntType,[BoolType])])
              |Err xs,OK BoolType->Err xs
              |Err xs,Err ls->Err (xs@ ls)
              |OK BoolType,Err xs->Err xs
              |OK IntType,Err xs->Err xs
              |Err xs,OK IntType->Err xs)
  |And(e1,e2) -> (match check e1 ctxt, check e2 ctxt with
              |OK BoolType,OK BoolType->OK BoolType
              |OK IntType,OK BoolType->
                Err ([IncorrectType(e1,IntType,[BoolType])])
              |OK BoolType,OK IntType->
                Err ([IncorrectType(e2,IntType,[BoolType])])
              |Err xs,OK BoolType->Err xs
              |Err xs,Err ls->Err (xs@ ls)
              |OK IntType,OK IntType->
                Err ((IncorrectType(e1,IntType,[BoolType])::
                [IncorrectType(e2,IntType,[BoolType])]))
              |OK BoolType,Err ls->Err ls
              |OK IntType,Err ls->
                Err ((IncorrectType(e1,IntType,[BoolType]))::ls)
              |Err ls,OK IntType->
                Err (ls@[(IncorrectType(e2,IntType,[BoolType]))]))
  |Not e->(match check e ctxt with
           |OK BoolType->OK BoolType
           |OK IntType->Err ([IncorrectType(e,IntType,[BoolType])])
           |Err ls->Err ls)
  |Let(str,bexpr,body)->(match check bexpr ctxt with
                        |OK bexpr_v->check body ((str,bexpr_v)::ctxt)
                        |_->check bexpr ctxt)





let e8 = Div (Val (Int 5), Val (Int 0))

let () =
  assert (evaluate e1 = OK (Int 7));
  assert (evaluate e2 = OK (Int 7));
  assert (evaluate e3 = OK (Bool true));
  assert (evaluate e4 = OK (Bool false));
  assert (evaluate e5 = OK (Bool true));
  assert (evaluate e6 = OK (Int 15));
  assert (evaluate e7 = OK (Bool true))

  let has_eval_errors (e:expr) : bool =
    match evaluate e with
    | OK _ -> false
    | Err _ -> true

  let () =
    assert (has_eval_errors er1);
    assert (has_eval_errors er2);
    assert (has_eval_errors er3);
    assert (has_eval_errors er4);
    assert (has_eval_errors er5);
    assert (has_eval_errors er6);
    assert (has_eval_errors er7)

let has_type_errors (e:expr) : bool =
    match check e [] with
    | OK _ -> false
    | Err _ -> true

  let () =
    assert (not (has_type_errors e8))

  let () =
    assert (has_type_errors er1);
    assert (has_type_errors er2);
    assert (has_type_errors er3);
    assert (has_type_errors er4);
    assert (has_type_errors er5);
    (* er6 has not type error *)
    assert (has_type_errors er7)



  let () =
    print_endline ("Success! All tests passed.")
