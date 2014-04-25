(* Voici les fonctions utilisées pour l'algorithme d'arc consistance pour les
 * contraintes alldiff *)

open Printf

(* Déclaration des types *)

type node = Node of int;;
type edge = Edge of node * node;;
type graph = Graph of node list * node list * edge list;;
type matching = Matching of edge list;;

(* Les fonctions secondaires *)

(* neighbor : node -> graph -> node list 
 * neighbor x g prend le sommet x et renvoie la liste de ses voisins dans g 
 * neighbor_aux : node -> graph -> node list -> node list *)
let neighbors n (Graph(x,y,e)) =
  let rec neighbors_aux x (Graph(x,y,e)) neighbors_set =
    match e with
    | [] -> neighbors_set
    | Edge(s,yn)::t -> if s = n
        then neighbors_aux n (Graph (x,y,t)) (yn::neighbors_set)
        else neighbors_aux n (Graph (x,y,t)) neighbors_set
  in
  neighbors_aux n (Graph(x,y,e)) []
;;

(* find_first_not : node list -> node list -> node 
 * find_first_not l1 l2 renvoie le premier element de l2 qui n'est pas dans l1
 *)
let find_first_not l1 l2 = List.find (fun el -> (not (List.memq el l1))) l2
;;

(* Les fonctions principales *)

let matching (Graph(x,y,e)) =
  let rec matching_aux (x,y,e) matchingNodes matchingEdges = match x with
    | [] -> (matchingNodes, matchingEdges)
    | elx::t -> 
      try
        let ely = (find_first_not matchingNodes (neighbors elx (Graph(x,y,e)))) in
        matching_aux (t,y,e) (elx::ely::matchingNodes) ((elx,ely)::matchingEdges)     
      with
        Not_found -> matching_aux (t,y,e) matchingNodes matchingEdges
  in
  matching_aux (x,y,e) [] []
;;



