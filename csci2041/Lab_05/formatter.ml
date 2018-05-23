(*work with Xingtong Zhang,Yuze Jiang,Youya Xia*)

let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "
let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

let read_file (file_name: string) : char list =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

(*add three types to make the type passed into my function become clear*)
type words_file=char list list
type line=string
type input_file=char list

(*define the input and output type for this funciton and change indentation*)
let split f (xs:input_file):words_file
  =let split_helper hd accum=
  match accum with
    |_ when f hd->[]::accum
    |x1::list1->(hd::x1)::list1
    |[]->[[hd]]
    in List.fold_right split_helper xs [[]]

let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i =
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

let counter xs=List.fold_left(fun accum hd->if hd='\n' then accum+1 else accum) 0 xs

(*define the input and output type for this funciton and change indentation*)
let format_helper (s1:line) length:line=
  let temp=split (fun x->(x=' ')||(x='\n')||(x='\r')||(x='\t')) (explode s1) in
    let helper accum hd= if List.length hd=0 then accum
          else if counter(explode accum)=0 && (String.length accum +String.length (implode hd))<=length
          then accum^(implode hd)^" "
          else if not (counter(explode accum)=0)&& ((String.length accum-String.rindex accum '\n'-1)+String.length(implode hd)<length)
          then accum^(implode hd)^" "
          else if not (counter(explode accum)=0)&& ((String.length accum-String.rindex accum '\n'-1)+String.length(implode hd)=length)
          then accum^(implode hd)^"\n"
          else String.trim(accum)^"\n"^(implode hd)^" "
      in List.fold_left helper "" temp

(*define the input and output type for this funciton*)
let format (s1:line) length:line=String.trim(format_helper s1 length)
