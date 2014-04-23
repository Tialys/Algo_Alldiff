(* Nouveaux testes *)

open Printf

(* Les différents type *)
type node = Node of int;;
type edge = Edge of node * node;;
type graphe = Graphe of node list * node list * edge list;;

(* Les fonctions sur le type graphe *)

let x_g g = match g with
	| Graphe (x,_,_) -> x
;;

let y_g g = match g with
	| Graphe (_,y,_) -> y
;;

let e_g g = match g with
	| Graphe (_,_,e) -> e
;; 

(* Les fonctions de voisinages *)

let rec first_voisin_ens x ens_e = match ens_e with
	| [] -> Node(-1) 
	| Edge(s,y)::e -> 
		if s = x
		then y
		else first_voisin_ens x e
;; 

let rec voisin_ens x ens_e acc = match ens_e with
        | [] -> acc
        | Edge(s,y)::e ->
                if s = x
                then voisin_ens x e (y::acc)
                else voisin_ens x e acc 
;; 


let voisin x g = voisin_ens x (e_g g) []
;;
(*
let rec find p l = match l with
	| [] -> []
	| x::q -> if p x then (x::[]) else find p l
;;
*)

let rec nmem x l = match l with
	| [] -> false 
	| a::q -> a = x || nmem x q
;;

let first_voisin x g = first_voisin_ens x (e_g g)
;;

let rec vrai_y_ens x ens_e node_sat = match ens_e with
        | [] -> Node(-1)
        | Edge(s,y)::e ->
                if ((s = x) && (not (nmem y node_sat)))
                then y
                else vrai_y_ens x e node_sat
;;

let vrai_y x g node_sat = vrai_y_ens x (e_g g) node_sat
;;

(* Les fonctions de couplage *)

let rec couplage_ens g ens_x node_sat edge_sat = match ens_x with 
	| [] -> (node_sat, edge_sat)
	| x::rx -> 
		let y = vrai_y x g node_sat in match y with
		| Node(n) -> if (n == -1)
		then couplage_ens g rx node_sat edge_sat
		else couplage_ens g rx (x::y::node_sat) ((x,y)::edge_sat)
;;

let couplage g = couplage_ens g (x_g g) [] []
;;

let rec find_all l ens acc = match l with
	| [] -> acc
	| x :: lq -> if (not (nmem x ens)) then find_all lq ens (x::acc) 
		else find_all lq ens acc
;;

let trouve l1 l2 = find_all l1 l2 []
;;

let rec dfs x g stack acc = let vx = trouve (voisin x g) stack in
	let new_stack = vx @ stack in
	match new_stack with 
	| [] -> acc
	| el::q -> dfs el g new_stack (el::x::acc)
;;

let remove ca = match ca with
	| [] -> []
	| a::q -> q
;;

let rec x_couple y edge_sat = match edge_sat with
	| [] -> y
	| (x,s)::e -> if (s = y) then x else x_couple y e
;;

let rec chaine_alternante_stack x g stack ca edge_sat node_sat acc =
        let vx = trouve (voisin x g) (acc) in
	match vx with
	| [] -> (match stack with
		| [] -> []
		| _ -> (let new_ca = remove ca in match new_ca with
			| el::q -> chaine_alternante_stack el g stack (remove new_ca) edge_sat node_sat acc
			| _ -> []))
	| y::qvx -> if (nmem y node_sat)
		then let new_x = x_couple y edge_sat in
			 chaine_alternante_stack new_x g (qvx @ stack) (y::x::ca) edge_sat node_sat (y::acc)
		else y::x::ca
;; 

let chaine_alternante x g edge_sat node_sat = chaine_alternante_stack x g [] [] edge_sat node_sat []
;;

let printnoeud n = match n with
        |Node(no) -> printf "%d" no
;;


let rec printn_list = function
	| [] -> ()
	| e::l -> printnoeud e; print_string " "; printn_list l
;;


let graphe1 = Graphe([Node(1);Node(2);Node(3);Node(4);Node(5);Node(6)],[Node(7);Node(8);Node(9);Node(10);Node(11);Node(12);Node(13)],[Edge((Node(1),Node(7)));Edge((Node(1),Node(8)));Edge((Node(2),Node(8)));Edge((Node(2),Node(9)));Edge((Node(3),Node(7)));(*Edge((Node(3),Node(9)));*)Edge((Node(4),Node(8)));Edge((Node(4),Node(10)));Edge((Node(5),Node(9)));Edge((Node(5),Node(10)));Edge((Node(5),Node(11)));Edge((Node(5),Node(12)));Edge((Node(6),Node(12)));Edge((Node(6),Node(13)))])
;;

let graphe2 = Graphe([Node(1);Node(2);Node(3);Node(4)],[Node(1);Node(2);Node(3);Node(4)], [Edge((Node(1),Node(1)));Edge((Node(1),Node(2)));Edge((Node(2),Node(1)));Edge((Node(2),Node(2)));Edge((Node(3),Node(2)));Edge((Node(3),Node(3)));Edge((Node(4),Node(3)));Edge((Node(4),Node(4)))])
;;

let graphe3 = Graphe([Node(1);Node(2);Node(3)],[Node(4);Node(5);Node(6)],[Edge((Node(1),Node(4)));Edge((Node(1),Node(5)));Edge((Node(1),Node(6)));Edge((Node(2),Node(4)));Edge((Node(2),Node(6)));Edge((Node(3),Node(6)))])
;;


let (noeud_sat1, edge_sat1) = couplage graphe1
;;
let (noeud_sat2, edge_sat2) = (remove (remove noeud_sat1), remove edge_sat1)
;;



printn_list (y_g graphe1)
;;
printf "\n"
;;
printn_list (x_g graphe1)
;;
printf "\n"
;;
printnoeud (first_voisin (Node(5)) graphe1)
;;
printf "\n"
;;
printn_list (voisin (Node(5)) graphe1)
;;
printf "\n"
;;
printnoeud (vrai_y (Node(5)) graphe1 [Node(7);Node(8);Node(10);Node(9)])
;;
printf "\n"
;;
printf "%B" (nmem (Node(5)) [Node(1)])
;;
printf "\n"
;;
printn_list (noeud_sat1)
;;
printf "\n"
;;
printn_list (chaine_alternante (Node(3)) graphe1 edge_sat1 noeud_sat1)
;;
printf "\n"
;;
printn_list (trouve (voisin (Node(3)) graphe1) ([]@[]))
;;
printf "\n"
;;
printf "%B" (not (nmem (Node(3)) noeud_sat1))
;;
printf "\n"
;;
printnoeud ( x_couple (Node(7)) edge_sat1)
;;
printf "\n"
;;

(*


let rec couplage_max g ens_x node_sat edge_sat = 
eud (first_voisin (Node(1)) graphe1)
;;
	match ens_x with
	| [] -> (node_sat, edge_sat)
	| x::q -> let (new_node_sat, new_edge_sat) = 
		upgrade_couplage g chaine_alternante x g node_sat edge_sat in			couplage_max g q new_node_sat new_edge_sat
;;

*)




printnoeud (Node(1))
;;
