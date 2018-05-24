(*compute the and result lazily*)
let rec ands (ls:bool list):bool=
match ls with
|[]->true
|hd::tl->if hd then ands tl else false
