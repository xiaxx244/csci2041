(*this part below is to format a string based on restricted length*)
let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "
let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

(*read_file function cited from homework requirement to test my functions*)
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

(*implode function below cited from homework requirement to convert char list to string*)
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

(*this split method below is to remove all white space,\r,\t,\n in the text file*)
(*type:('a -> bool) -> 'a list -> 'a list list*)
let split f xs=let split_helper hd accum=
            match hd,accum with
            |hd,_ when f hd->[]::accum
            |hd,(x1::list1)->(hd::x1)::list1
            |hd,[]->[[hd]]
            in List.fold_right split_helper xs [[]]

(*this method below is cited from homework requirement to convert string to char list*)
let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i =
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

(*this method below is to count the number of \n in the list*)
(*since the String.rindex method will raise an exception if \n is not found,so I use this method to determine if the the accumulator has switched to a new line*)
(*type:char list -> int*)
let counter xs=List.fold_left(fun accum hd->if hd='\n' then accum+1 else accum) 0 xs

(*this helper method below is to format a text based on the restricted legnth*)
(*do not concat string the accumulator if the list is empty*)
(*if the length from the last \n to the last letter in the accumulator is less than the target length,then just concat the string to the accumulator and add a white space between two words*)
(*else since I have add a white space at the end of the old line so,using String.trim to remove the extra white space and add\n to the accumulator and then concat the string and add a white space between two words*)
(*type:string -> int -> string*)
let format_helper s1 length=
let temp=split (fun x->(x=' ')||(x='\n')||(x='\r')||(x='\t')) (explode s1) in
let helper accum hd= if List.length hd=0 then accum
          else if counter(explode accum)=0 && (String.length accum +String.length (implode hd))<=length then accum^(implode hd)^" "
          else if not (counter(explode accum)=0)&& ((String.length accum-String.rindex accum '\n'-1)+String.length(implode hd)<length) then accum^(implode hd)^" "
          else if not (counter(explode accum)=0)&& ((String.length accum-String.rindex accum '\n'-1)+String.length(implode hd)=length) then accum^(implode hd)^"\n"
          else String.trim(accum)^"\n"^(implode hd)^" "
in List.fold_left helper "" temp

(*the format function to format a string*)
(*use String.trim to get rid of the extra white space at the end of the formatted string*)
(*type:string -> int -> string*)
let format s1 length=String.trim(format_helper s1 length)
