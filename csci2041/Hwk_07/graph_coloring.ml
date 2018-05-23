(*explanation:Choose color_option function for explanation:
First, I numbered the 3 colors as C 1,C 2 and C 3)
1.search space:since we need to color all nodes,so the search space is
the node list in the graph;
In each step,when coloring each node,I first check
if the coloring has colored two adjacent node with the same color
using the helper function graph_checker,if this happens,then return None
since this partial solution definitely do not meet our requirements
and stop search on that path then;in the
else-if clause:if all of the nodes have been colored
and adjacent nodes have been colored with differenet colors,
then this indicates the solution is valid,and return the solution,
in the final else clause:if none of the above two conditions is true for
the current coloring graph,then match the remaining nodes,
and try color the head of the remaining nodes with C 1
and call the color_test function recursively to determine
whether or not the solution can exist with the head being colored C 1,
if a solution exists,then return the solution,else I continue
to try color the head with C 2 and C 3 in the remaining two match cases
to find out if the graph has the solution.
If a solution does not exist,then return None,else return the solution.
2.avoid inefficiency:I achieve this in two steps:
(1):in the color_option function,I first check
if the two adjacent node shares the same color,if that happens,then
I just return None and do not contiune my search on that path;
(2):in the helper function graph_checker,
in this helper function which is designed to check if the adjacent nodes
have the same color,when encounter this situation,
I avoid searching the rest of coloring
and return false directly*)

type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

let g1:graph=
( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )

let g1_coloring:coloring= [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]

(*this function is aimed to search for the color of a given node*)
let rec search (c:coloring) (n:node):color option=
  match c with
  |[]->None
  |(node,col)::rest->if node=n then Some col else search rest n

(*this function is designed to check if the two adjacent node
have the same color*)
let rec graph_checker (c:coloring) ((vertex,edge):graph):bool=
  match edge with
  |[]->true
  |(node1,node2)::rest->if search c node1=search c node2
                      &&  (not (search c node1=None))
                      then false
                      else graph_checker c (vertex,rest)

(*this function is designed to use option type to check is
a given graph can have a solution with 3 color*)
let color_option ((vertex,edge):graph):coloring option=
  let rec color_test (partial_color:coloring)
  ((vertex,edge):graph):coloring option =
    if not (graph_checker partial_color (vertex,edge))
    then None
    else if (graph_checker partial_color (vertex,edge))
      && partial_color <>[] && vertex=[]
    then Some (List.rev partial_color)
    else
      match vertex with
      |[]->None
      |hd::tl->
          (match color_test ((hd,C 1)::partial_color) (tl,edge) with
          |Some result->Some result
          |None->(match color_test  ((hd,C 2)::partial_color) (tl,edge) with
                  |None->color_test  ((hd,C 3)::partial_color) (tl,edge)
                  |Some result->Some result)
          )
  in color_test [] (vertex,edge)

  exception FoundColoring of coloring

  (*this function is designed to use exception  to check is
  a given graph can have a solution with 3 color,if exist a solution,
  thne raise a exxception*)
  let rec color_exception ((vertex,edge):graph):unit=
    let rec color_test (partial_color:coloring)
    ((vertex,edge):graph):unit=
      if not (graph_checker partial_color (vertex,edge))
      then ()
      else if(graph_checker partial_color (vertex,edge))
        && partial_color <>[] && vertex=[]
      then raise (FoundColoring (List.rev partial_color))
      else
        match vertex with
        |[]->()
        |hd::tl->color_test ((hd,C 1)::partial_color) (tl,edge);
              color_test  ((hd,C 2)::partial_color) (tl,edge);
              color_test  ((hd,C 3)::partial_color) (tl,edge)
    in color_test [] (vertex,edge)
