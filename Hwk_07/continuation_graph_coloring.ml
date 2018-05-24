open Graph_coloring
open LazeeModules
open StreamModulesRedux
module LS = Stream_v2(Lazee_v1)
type coloring=Graph_coloring.coloring
let rec search (c:coloring) (n:node):color list=
  match c with
  |[]->[]
  |(node,col)::rest->if node=n then col::search rest n else search rest n

let rec color_test ls (partial_color:coloring)
((vertex,edge):graph) cnn=
    match vertex with
    |[]->if (LS.head cnn<>LS.head (LS.tail cnn)) then cnn
          else color_test
    |hd::tl->
        let rec help tar ls=
            match ls with
            |[]->cnn
            |hd1::tl1->
              if
              not (Graph_coloring.graph_checker ((tar,hd1)::partial_color) (vertex,edge))
              then help tar tl1
              else
            color_test ls partial_color (tl,edge)
            (LS.delay(fun ()-> LS.Cons(((tar,hd1)::LS.head cnn),LS.tail cnn)))
        in help hd ls

  let temp=[]
  let rec k_color_continuation ls (vertex,edge):coloring LS.t=
  let cnn=LS.delay(fun ()->LS.Cons([],k_color_continuation ls (vertex,edge)))
  in
  color_test ls [] (vertex,edge) cnn temp
