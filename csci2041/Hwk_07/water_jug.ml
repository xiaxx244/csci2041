(*I create a type called state which uses int*int to represent the amount of
water in each jug*)
type state=int*int

type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug

(*I use a type called ok_state to indeicate the state which the
3 gallon jug is empty and the 4 gallon jug contains 2 gallons*)
let ok_state ((t3,t4):state):bool=
(t3=0) && (t4=2)

(*this following function is aimed to use option type to define a
valid move,if a move is not valid for given state,then return None,
else update the state and the operation*)
let valid_move (p:operation) ((s3,s4):state):(operation * state) option=
  match p with
  |Fill4GallonJugFromTap->if s4<4 then Some (Fill4GallonJugFromTap,(s3,4))
                                  else None
  |Fill3GallonJugFromTap->if s3<3 then Some (Fill3GallonJugFromTap,(3,s4))
                                else None
  |Empty4GallonJugOnGround->if s4>0
                          then Some (Empty4GallonJugOnGround,(s3,0))
                          else None
  |Empty3GallonJugOnGround->if s3>0 then
                                  Some (Empty3GallonJugOnGround,(0,s4))
                                  else None
  |Fill4GallonJugFrom3GallonJug->
                            if s3>0 && s4+s3>=4 then
                            Some(Fill4GallonJugFrom3GallonJug,((s3+s4)-4,4))
                            else None
  |Fill3GallonJugFrom4GallonJug->
                              if s4>0 && s4+s3>=3 then
                              Some(Fill3GallonJugFrom4GallonJug,(3,(s3+s4)-3))
                              else None
  |Empty4GallonJugInto3GallonJug->
                              if (s3+s4<=3) && s4>0
                              then
                              Some (Empty4GallonJugInto3GallonJug,(s3+s4,0))
                              else None
  |Empty3GallonJugInto4GallonJug->if (s3+s4<=4) && s3>0
                                then
                                Some(Empty3GallonJugInto4GallonJug,(0,s3+s4))
                                else None

(*this is a function provided by the hw to
convert state to a readable string*)
let describe (four:int) (three:int) : string =
        let describe' jug amount =
                "The " ^ string_of_int jug ^ " gallon jug " ^
                match amount with
                  | 0 -> " is empty"
                | 1 -> " contains 1 gallon"
                | x -> " contains " ^ string_of_int x ^ " gallons"
          in
        describe' 4 four ^ ", " ^ describe' 3 three ^ "."

(*this is a function to check is a a valid_operation
leads to the two jugs to return to a previous state,
if so then return false*)
let rec check ((op1,(s3,s4)):(operation*state))
(p:(operation*string) list):bool=
  match p with
  |[]->true
  |((op,s)::rest)->if s=(describe s4 s3) then false
                    else if (describe s4 s3)=(describe 0 0)
                    then false
                    else check (op1,(s3,s4)) rest

(*this is a function to move the jugs using the operations defined above
I put all operations in a oplist,if a operation is a valid_move and
it will not lead to the jugs to return to a previous state
then update the state,else continue to search for other operations
to find a operation that meet our requirement,
if we search the whole graph and still can not find a valid operation,
then just return None*)
let rec move (s:state) (p:operation list) (ls:(operation*string) list)
:(operation*state) option=
  match p with
  |[]->None
  |hd::tl->(match valid_move hd s with
            |None->move s tl ls
            |Some result when not (check result ls)-> move s tl ls
            |Some result->Some result)


let oplist:operation list=[Fill4GallonJugFromTap;
            Fill3GallonJugFromTap;
            Empty4GallonJugOnGround;
            Empty3GallonJugOnGround;
            Fill4GallonJugFrom3GallonJug;
            Fill3GallonJugFrom4GallonJug;
            Empty4GallonJugInto3GallonJug;
            Empty3GallonJugInto4GallonJug]

(*this is a function to move the jugs to the ok_state,
the search space is all of the eight given operations,and if the move
of jug from its current state using one of the eight operations
is valid then we update its state and
continue to move the two jugs until te jugs reach the ok_state,else just
return None since all eight operations cannot lead to a valid move*)
let play ():(operation * string) list option=
  let rec go_from (state:state) (path:(operation*string) list)
  :(operation*string) list option  =
    if ok_state state then Some path
    else
     match move state oplist path with
     |None->None
     |Some (op,(s3,s4))->
                        go_from (s3,s4) (path@[(op,(describe s4 s3))])
  in go_from (0,0) []
