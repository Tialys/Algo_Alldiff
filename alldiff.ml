(* Voici les fonctions utilisées pour l'algorithme d'arc consistance pour les
 * contraintes alldiff *)

open Printf

(* Déclaration des types *)

type node = Node of int;;
type edge = Edge of node * node;;
type graph = Graph of node list * node list * edge list;;

(* Les fonctions secondaires *)


(* neighbor_aux : node -> graph -> node list -> node list *)
let rec neighbor_aux x (Graph(x_set,y_set,e_set)) neighbor_set =
	match e_set with
	| [] -> neighbor_set
	| Edge(s,y)::t -> if s = x
		then neighbor_aux x (Graph (x_set,y_set,t)) (y::neighbor_set)
		else neighbor_aux x (Graph (x_set,y_set,t)) neighbor_set
;;

(* neighbor : node -> graph -> node list 
 * neighbor x g prend le sommet x et renvoie la liste de ses voisins dans g *)
let neighbor x g = neighbor_aux x g []
;;

(* find_first_not : node list -> node list -> node 
 * find_first_not l1 l2 renvoie le premier element de l2 qui n'est pas dans l1
 *)
let find_first_not l1 l2 = List.find (fun el -> (not (List.memq el l1))) l2
;;


(* Les fonctions principales *)
(*
let rec matching_aux g mnode medge = let (x_set,y_set,e_set) = g in
	match x_set with
	| [] -> (mnode, medge)
	| x::t -> let y = find_first_not mnode (neighbor x g)
*)


