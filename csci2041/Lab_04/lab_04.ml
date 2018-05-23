let length xs=List.fold_left(fun accum x->accum+1) 0 xs

let andf xs=List.fold_left(fun accum x->x&&accum) true xs

let orf xs=List.fold_left(fun accum x->x || accum) false xs

let is_elem a xs=List.fold_left(fun accum x->if a=x then true else (false || accum )) false xs

let rev xs=List.fold_left(fun accum x->x::accum) [] xs

let ascii_sum xs=List.fold_left(fun accum x->accum+Char.code x) 0 xs

let lebowski xs=List.fold_right(fun x accum->if x='.' then [','; ' '; 'd'; 'u'; 'd'; 'e'; '.'] @ accum else x::accum) xs []
