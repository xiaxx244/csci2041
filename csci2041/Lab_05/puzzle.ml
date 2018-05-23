(*work with Xingtong Zhang,Yuze Jiang,Youya Xia*)

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

(*add four types to make the type passed into my function become clear*)
type words=string list
type words_pair= (string*string) list
type input_file=char list
type words_file=char list list

(*define the input and output type for this function and change indentation*)
let split remove_whitespace (words_list:input_file):words_file=
  let split_helper hd accum=
    match accum with
      |_ when remove_whitespace hd->[]::accum
      |x1::list1->(hd::x1)::list1
      |[]->[[hd]]
      in List.fold_right split_helper words_list [[]]

(*define the input and output type for this funciton*)
(*use words_list instead of xs to represent argument in the function*)
let extract len (words_list:words_file):words=
  let extract_helper hd accum=
  if List.length hd=len then ((implode hd)::accum) else accum
  in List.fold_right extract_helper words_list []

(*define the input and output type for this function and change indentation*)
let get_answer (list1:words) (list2:words):words=
  let helper hd accum=if List.length(List.filter (fun x->(String.lowercase_ascii x)=String.lowercase_ascii (String.sub hd 1 4)) list2)=0 then accum else (hd::accum)
  in List.fold_right helper list1 []

(*define the input and output type for this funciton*)
(*use file_path instaed of s1 to represent the string representation of a filename*)
(*use words6 and words4 to represent the list of words of length 6 and 4*)
let answers (file_path:string):words=
    let words6=extract 6 (split(fun x->(x=' ')||(x='\n')||(x='\r')||(x='\t')) (read_file file_path)) in
    let words4=extract 4 (split(fun x->(x=' ')||(x='\n')||(x='\r')||(x='\t')) (read_file file_path)) in
    get_answer words6 words4

(*define the input and output type for this funciton*)
let pretty_answers (xs:words):words_pair=
  let phelper hd accum=(String.sub hd 1 4,hd)::accum
  in List.fold_right phelper xs []


let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"
