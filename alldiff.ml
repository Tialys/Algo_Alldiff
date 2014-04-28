(* Voici les fonctions utilisées pour l'algorithme d'arc consistance pour les
 * contraintes alldiff *)

open List



type node = Node of int;;
type edge = Edge of node * node;;
type graph = Graph of node list * node list * edge list;;
type matching = Matching of edge list;;



(* neighbors_x : node -> graph -> node list 
 * neighbors_x x g prend le sommet x de X et renvoie la liste de ses voisins 
 * dans g(X,Y,E) 
 * neighbor_x' : node -> graph -> node list -> node list *)
let neighbors_x g n =
  let rec neighbors_x' x (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> if i = n
        then neighbors_x' n (Graph (x,y,t)) (j::ns)
        else neighbors_x' n (Graph (x,y,t)) ns
  in
  neighbors_x' n g []
;;


(* neighbors_y : node -> graph -> node list 
 * neighbors_y y g prend le sommet y de Y et renvoie la liste de ses voisins 
 * dans g(X,Y,E) 
 * neighbor_y' : node -> graph -> node list -> node list *)
let neighbors_y g n =
  let rec neighbors_y' x (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> if j = n
        then neighbors_y' n (Graph (x,y,t)) (j::ns)
        else neighbors_y' n (Graph (x,y,t)) ns
  in
  neighbors_y' n g []
;;

(* remove_edge : node -> edge list -> edge list
 * remove_edge n es enlève l'arrete (n,y) et l'arrette (x,y) de la liste es *)
let rec remove_edge n es = match es with
  | [] -> []
  | (x,y)::es -> if n = x then remove_edge y es else 
    if n = y then es else remove_edge n es
;;

(* x_matching : node -> edge list -> node
 * x_matching y es renvoie le sommet x couplé à l'arrette y dans la liste es *)
let rec x_matching y es = match es with
        | [] -> y
        | (x,s)::e -> if (s = y) then x else x_matching y e
;;

(* remove : node -> node list -> node
 * remove e l  renvoie la liste l privée l'élément e *)
let remove e l = 
  let rec remove' e l aux = match l with
    | [] -> aux
    | el::t -> if e = el then (aux @ t) else remove' e t (el::aux)
  in
  remove' e l []
;;

(* upgrade_matching : edge list -> edge list -> edge list
 * upgrade_matching matchingEdges path *) 
let rec upgrade_matching matchingEdges path = match path with
  | [] -> matchingEdges
  | el::t -> if (mem el matchingEdges) 
      then upgrade_matching (remove el matchingEdges) t 
      else upgrade_matching (el::matchingEdges) t
;;

(*
let dfs_edge y g =
  let rec dfs_edge' y g stack visited edges =
    match ((filter (fun el -> (not (mem el stack))) (neighbors_y g y))@stack) with
    | [] -> edges
    | el::t -> if (mem el visited)dfs_edge new_y g 
*)

(* matching : graph -> (node list * edge list)
 * matching g renvoie un couplage du graphe g *)
let matching (Graph(x,y,e)) =
  let rec matching' (x,y,e) matchingNodes matchingEdges = match x with
    | [] -> (matchingNodes, matchingEdges)
    | elx::t -> 
      try
        let ely = (find (fun el -> (not (mem el matchingNodes))) (neighbors_x (Graph(x,y,e)) elx)) in
        matching' (t,y,e) (elx::ely::matchingNodes) ((elx,ely)::matchingEdges) 
      with
        Not_found -> matching' (t,y,e) matchingNodes matchingEdges
  in
  matching' (x,y,e) [] []
;;

(* alternating_path : node -> graph -> edge list -> node list -> edge list
 * alternating_path x g matchingEdges matchingNodes renvoie la chaine alternante
 * augmentante partant de x *)
let alternating_path x g matchingEdes matchingNodes =
  let rec alternating_path' x g matchingEdges matchingNodes visited path =
    match (filter (fun el -> (not (mem el visited))) (neighbors_x g x)) with
    | [] -> (let new_path = remove_edge x path in match new_path with
        | (new_x,_)::q -> alternating_path' new_x g matchingEdges matchingNodes visited new_path
        | _ -> [])
    | y::t -> if (mem y matchingNodes)
        then let new_x = x_matching y matchingEdges in
          alternating_path' new_x g matchingEdges matchingNodes (y::visited) ((new_x,y)::(x,y)::path)
        else (x,y)::path
  in
  alternating_path' x g matchingEdes matchingNodes [] []
;;

(* matching_max : graph -> edge list -> edge list
 * matching_max g matchingEdges renvoit le couplage max du graphe g *)
let rec matching_max (Graph(x,_,_) as g) matchingEdges =
  let (matchingNodesx, matchingNodesy) = split matchingEdges in
    match (filter (fun el -> (not (mem el matchingNodesx))) x) with
    | [] -> matchingEdges
    | el::xs -> let path = alternating_path el g matchingEdges (matchingNodesx @ matchingNodesy) in match path with
      | [] -> []
      | _ -> matching_max g (upgrade_matching matchingEdges path)
;;

(*
let dfs_used g matchingEdges =
  let rec dfs_used' (Graph(_,y,_) as g) matchingEdges usedEdges = 
    let (_, matchingNodesy) = split matchinEdges in
      match (filter (fun el -> (not (mem el g matchingNodesy))) y) with
      | [] -> usedEdge
      | el::t -> dfs_used' g matchingEdges (dfs_edge el g)
  in
  dfs_used' g matchingEdges []
;;
*)
