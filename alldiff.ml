(* Voici les fonctions utilisées pour l'algorithme d'arc consistance pour les
 * contraintes alldiff *)

open List



type nodeX = X of int;;
type nodeY = Y of int;;
type edge = Edge of nodeX * nodeY;;
type graph = Graph of nodeX list * nodeY list * edge list;;
type matching = Matching of edge list;;



(* neighbors_x : node -> graph -> node list 
 * neighbors_x x g prend le sommet x de X et renvoie la liste de ses voisins 
 * dans g(X,Y,E) 
 * neighbor_x' : node -> graph -> node list -> node list *)
let neighborsX g n =
  let rec neighborsX' n (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> neighborsX' n (Graph (x,y,t)) (if i = n then (j::ns) else ns)
  in
  neighborsX' n g []
;;


(* neighbors_y : node -> graph -> node list 
 * neighbors_y y g prend le sommet y de Y et renvoie la liste de ses voisins 
 * dans g(X,Y,E) 
 * neighbor_y' : node -> graph -> node list -> node list *)
let neighborsY g n =
  let rec neighborsY' n (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> neighborsY' n (Graph (x,y,t)) (if j = n then (i::ns) else ns)
  in
  neighborsY' n g []
;;

(* remove_edge : node -> edge list -> edge list
 * remove_edge n es enlève l'arrete (n,y) et l'arrette (x,y) de la liste es *)
let rec removeEdge n es = match es with
  | [] -> []
  | (x,y)::es -> if n = x then removeEdge y es else 
    if n = y then es else removeEdge n es
;;

(* x_matching : node -> edge list -> node
 * x_matching y es renvoie le sommet x couplé à l'arrette y dans la liste es *)
let rec xMatching y es = match es with
        | [] -> raise Not_found
        | (x,s)::e -> if (s = y) then x else xMatching y e
;;

(* x_matching : node -> edge list -> node
 * x_matching y es renvoie le sommet x couplé à l'arrette y dans la liste es *)
let rec yMatching x es = match es with
        | [] -> raise Not_found
        | (s,y)::e -> if (s = x) then y else yMatching x e
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
(*
let alternating path =
  let alternating' path aux = match path with
    | [] -> aux
    | (x,y)::t ->*) 

(* upgrade_matching : edge list -> edge list -> edge list
 * upgrade_matching matchingEdges path *) 
let rec upgradeMatching matchingEdges path = match path with
  | [] -> matchingEdges
  | el::t -> if (mem el matchingEdges) 
      then upgradeMatching (remove el matchingEdges) t 
      else upgradeMatching (el::matchingEdges) t
;;

let rec used x l edgeUsed = match l with
  | [] -> edgeUsed
  | e::t -> if (mem (e,x) edgeUsed) then used x t edgeUsed else used x t ((e,x)::edgeUsed)
;;
(*
let rec fundkdsl y g stack visited edgeUsed matchingEdge =
  let ny = neighborsY g y in let newEdgeUsed = used y ny edgeUsed in
    let newStack = ((filter (fun el -> (not (mem el (visited@stack)))) ny)@stack) in
      match newStack with
      | [] -> edgeUsed
      | e::t -> fundkdsl (yMatching e matchingEdge) g t (y::visited) newEdgeUsed matchingEdge
;;
*)

let dfsY y g matchingEdges =
  let rec dfs' y g matchingEdges stack (visitedX, visitedY) =
    let newStack = (filter (fun el -> (not (mem el (stack@visitedX)))) (neighborsY g y))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> dfs' (yMatching el matchingEdges) g matchingEdges t (el::visitedX, y::visitedY)
    in
    dfs' y g matchingEdges [] ([], [])
;;

let revDfsX x g matchingEdges=
  let rec dfs' x g matchingEdges stack (visitedX, visitedY) =
    let newStack = (filter (fun el -> (not (mem el (stack@visitedY)))) (neighborsX g x))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> dfs' (xMatching el matchingEdges) g matchingEdges t (x::visitedX, el::visitedY)
    in
    dfs' x g matchingEdges [] ([], [])
;;

let dfsX x g matchingEdges =
  let rec dfs' y g matchingEdges stack (visitedX, visitedY) =
    let newStack = (filter (fun el -> (not (mem el (stack@visitedX)))) (neighborsY g y))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> dfs' (yMatching el matchingEdges) g matchingEdges t (el::visitedX, y::visitedY)
    in
    dfs' (yMatching x matchingEdges) g matchingEdges [] ([x], [])
;;

let revDfsY y g matchingEdges=
  let rec dfs' x g matchingEdges stack (visitedX, visitedY) =
    let newStack = (filter (fun el -> (not (mem el (stack@visitedY)))) (neighborsX g x))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> dfs' (xMatching el matchingEdges) g matchingEdges t (x::visitedX, el::visitedY)
    in
    dfs' (xMatching y matchingEdges) g matchingEdges [] ([], [y])
;;


(* matching : graph -> (node list * edge list)
 * matching g renvoie un couplage du graphe g *)
let matching (Graph(x,y,e)) =
  let rec matching' (x,y,e) matchingNodes matchingEdges = match x with
    | [] -> (matchingNodes, matchingEdges)
    | elx::t -> 
      try
        let ely = (find (fun el -> (not (mem el matchingNodes))) (neighborsX (Graph(x,y,e)) elx)) in
        matching' (t,y,e) (ely::matchingNodes) ((elx,ely)::matchingEdges) 
      with
        Not_found -> matching' (t,y,e) matchingNodes matchingEdges
  in
  matching' (x,y,e) [] []
;;

(* alternating_path : node -> graph -> edge list -> node list -> edge list
 * alternating_path x g matchingEdges matchingNodes renvoie la chaine alternante
 * augmentante partant de x *)
let alternatingPath x g matchingEdes matchingNodes =
  let rec alternatingPath' x g matchingEdges matchingNodes visited  =
    match filter (fun el -> (not (mem el visited))) (neighborsX g x) with
    | [] -> raise Not_found
    | [y] -> if (mem y matchingNodes)
        then let new_x = xMatching y matchingEdges in
          (new_x,y)::(x,y)::(alternatingPath' new_x g matchingEdges matchingNodes (y::visited))
        else (x,y)::[]
    | y::z::t -> if (mem y matchingNodes)
        then let new_x = xMatching y matchingEdges in
          try
            (new_x,y)::(x,y)::(alternatingPath' new_x g matchingEdges matchingNodes (y::visited))
          with
            Not_found -> alternatingPath' new_x g matchingEdges matchingNodes (y::visited)
        else (x,y)::[]
  in
  alternatingPath' x g matchingEdes matchingNodes []
;;

(* matching_max : graph -> edge list -> edge list
 * matching_max g matchingEdges renvoit le couplage max du graphe g *)
let rec matchingMax (Graph(x,_,_) as g) matchingEdges =
  let (matchingNodesX, matchingNodesY) = split matchingEdges in
    match (filter (fun el -> (not (mem el matchingNodesX))) x) with
    | [] -> matchingEdges
    | el::xs -> let path = alternatingPath el g matchingEdges matchingNodesY in 
      match path with
      | [] -> []
      | _ -> matchingMax g (upgradeMatching matchingEdges path)
;;
(*
let rec strongComponents (Graph(x,_,_) as g) components =
  match filter (fun el -> (not (mem el components))) x with
  | [] -> components
  | el::t -> let 
*)
