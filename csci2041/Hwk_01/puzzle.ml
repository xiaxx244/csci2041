(*this part is to process the text file to select all words of length 6 in the text whose inner 4 letters match one of the words of length 4 in the input text*)

(*this method below is from the writtenup of the homework which is to read file from a file_path*)
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

(*this method below is from the writtenup requirement of homework to convert char list to a string*)
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

(*this split function below can help me to get rid of empty spaces in the text file*)
(*type:('a -> bool) -> 'a list -> 'a list list*)
let split f xs=let split_helper hd accum=
            match hd,accum with
            |hd,_ when f hd->[]::accum
            |hd,(x1::list1)->(hd::x1)::list1
            |hd,[]->[[hd]]
            in List.fold_right split_helper xs [[]]

(*this method below is to extract words of given length from a char list list and return a String list*)
(*type: int -> char list list -> string list*)
let extract len xs=
let extract_helper hd accum=if List.length hd=len then ((implode hd)::accum) else accum
in List.fold_right extract_helper xs []

(*this helper method below is to filter all of the words of length 6 whose inner 4 letters match one of the words of length 4 in the text file*)
(*put all of the wanted words in a String list*)
(*type:string list -> string list -> string list*)
(*treat uppercase as lowercase and ignore case sensitivity*)
let get_answer list1 list2=
let helper hd accum=if List.length(List.filter (fun x->(String.lowercase_ascii x)=String.lowercase_ascii (String.sub hd 1 4)) list2)=0 then accum else (hd::accum)
in List.fold_right helper list1 []

(*this method below is to use the helper function get_answer to extract all wanted words of length 6*)
(*type:string -> string list*)
let answers s1=
    let temp1=extract 6 (split(fun x->(x=' ')||(x='\n')||(x='\r')||(x='\t')) (read_file s1)) in
    let temp2=extract 4 (split(fun x->(x=' ')||(x='\n')||(x='\r')||(x='\t')) (read_file s1)) in
    get_answer temp1 temp2;;

(*this method below is to put the wanted words of legnth 6 and the matched words of length 4 in a tuple and put them in a list*)
(*type:string list -> (string * string) list*)
let pretty_answers xs=
let phelper hd accum=(String.sub hd 1 4,hd)::accum
in List.fold_right phelper xs []


let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"
