(* Voici les fonctions utilisées pour l'algorithme d'arc consistance pour les
 * contraintes alldiff *)

open List



type node = Node of int;;
type edge = Edge of node * node;;
type graph = Graph of node list * node list * edge list;;
type matching = Matching of edge list;;



(* neighbor : node -> graph -> node list 
 * neighbor x g prend le sommet x et renvoie la liste de ses voisins dans g 
 * neighbor_aux : node -> graph -> node list -> node list *)
let neighbors (Graph(x,y,e)) n =
  let rec neighbors' x (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> if i = n
        then neighbors' n (Graph (x,y,t)) (j::ns)
        else neighbors' n (Graph (x,y,t)) ns
  in
  neighbors' n (Graph(x,y,e)) []
;;

(* find_first_not : node list -> node list -> node 
 * find_first_not l1 l2 renvoie le premier element de l2 qui n'est pas dans l1
 *)





let matching (Graph(x,y,e)) =
  let rec matching' (x,y,e) matchingNodes matchingEdges = match x with
    | [] -> (matchingNodes, matchingEdges)
    | elx::t -> 
      try
        let ely = (find (fun el -> (not (memq el matchingNodes))) (neighbors (Graph(x,y,e)) elx)) in
        matching' (t,y,e) (elx::ely::matchingNodes) ((elx,ely)::matchingEdges)     
      with
        Not_found -> matching' (t,y,e) matchingNodes matchingEdges
  in
  matching' (x,y,e) [] []
;;



