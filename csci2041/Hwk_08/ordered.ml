
(*ordered module that will be used in this assignment*)
module type OrderedSig=sig
  type t
  val eq:t->t->bool
  val lt:t->t->bool
  val leq:t->t->bool
end

module Int:(OrderedSig with type t=int)=struct
  type t=int
  let eq a b=(a=b)
  let lt a b=a<b
  let leq a b=a<=b
end
