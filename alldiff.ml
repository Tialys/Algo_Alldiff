(* Voici les fonctions utilisées pour l'algorithme d'arc consistance pour les
 * contraintes alldiff *)

open List



type nodeX = X of int;;
type nodeY = Y of int;;
type node = NodeX of nodeX | NodeY of nodeY;;
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

(* upgrade_matching : edge list -> edge list -> edge list
 * upgrade_matching matchingEdges path *) 
let rec upgradeMatching matchingEdges path = match path with
  | [] -> matchingEdges
  | el::t -> if (mem el matchingEdges)
      then upgradeMatching (remove el matchingEdges) t 
      else upgradeMatching (el::matchingEdges) t
;;

let rec edgeList x l edgeUsed = match l with
  | [] -> edgeUsed
  | e::t -> if (mem (e,x) edgeUsed) then edgeList x t edgeUsed else edgeList x t ((e,x)::edgeUsed)
;;



let rec dfsEdges' y g matchingEdges stack (visitedX, visitedY) edges =
  try
    let notX = xMatching y matchingEdges in
    let newStack = (filter (fun el -> (not (mem el (notX::stack@visitedX)))) (neighborsY g y))@stack in
      match newStack with
      | [] -> edges
      | el::t -> let newY = yMatching el matchingEdges in
          dfsEdges' newY g matchingEdges t
          (if (mem newY visitedY) then (el::visitedX, visitedY)
          else (el::visitedX, newY::visitedY)) ((el, newY)::(el, y)::edges)
  with
    Not_found -> edges
;;

let dfsEdgesX x g matchingEdges = 
    let newY = yMatching x matchingEdges in
    dfsEdges' newY g matchingEdges [] ([x], [newY]) [(x, newY)]
;;

let rec dfsEdgesl l g matchingEdges edges =
  match l with
  | [] -> edges
  | el::t -> let newEdges = dfsEdgesX el g matchingEdges in
      dfsEdgesl t g matchingEdges (newEdges@edges)
;;

let dfsEdges y g matchingEdges = let ny = neighborsY g y in
  dfsEdgesl ny g matchingEdges (edgeList y ny [])
;;

let dfsUsed l g matchingEdges =
  let rec dfsUsed' l g matchingEdges edges = match l with
    | [] -> edges
    | y::t -> dfsUsed' t g matchingEdges ((dfsEdges y g matchingEdges)@edges)
  in
  dfsUsed' l g matchingEdges []
;;

let rec dfs' y g matchingEdges stack (visitedX, visitedY) =
  try
    let notX = xMatching y matchingEdges in
    let newStack = (filter (fun el -> (not (mem el (notX::stack@visitedX)))) (neighborsY g y))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> let newY = yMatching el matchingEdges in
          dfs' newY g matchingEdges t 
          (if (mem newY visitedY) then (el::visitedX, visitedY) 
          else (el::visitedX, newY::visitedY))
  with
    Not_found -> (visitedX, visitedY)
;;

let rec revDfs' x g matchingEdges stack (visitedX, visitedY) =
  try
    let notY = yMatching x matchingEdges in
    let newStack = (filter (fun el -> (not (mem el (notY::stack@visitedY)))) (neighborsX g x))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> let newX = xMatching el matchingEdges in
          revDfs' newX g matchingEdges t 
          (if (mem newX visitedX) then (visitedX, el::visitedY) 
          else (newX::visitedX, el::visitedY))
  with
    Not_found -> (visitedX, visitedY)
;;



let dfsY y g matchingEdges = dfs' y g matchingEdges [] ([], [y])
;;

let revDfsX x g matchingEdges = revDfs' x g matchingEdges [] ([x], [])
;;

let dfsX x g matchingEdges = try
    let newY = yMatching x matchingEdges in
    dfs' newY g matchingEdges [] ([x], [newY])
  with
    Not_found -> ([x], [])
;;

let revDfsY y g matchingEdges = try
    let newX = xMatching y matchingEdges in
    revDfs' newX g matchingEdges [] ([newX], [y])
  with
    Not_found -> ([], [y])
;;

let inter l1 l2 =
  let rec inter' (l1x,l1y) (l2x,l2y) (accx,accy) = match (l1x,l1y) with
    | ([],[]) -> (accx,accy)
    | (elx::tx,[]) -> inter' (tx,[]) (l2x,l2y) (if (mem elx l2x) then (elx::accx,accy) else (accx,accy))
    | ([],ely::ty) -> inter' ([],ty) (l2x,l2y) (if (mem ely l2y) then (accx,ely::accy) else (accx,accy))
    | (elx::tx, ely::ty) -> inter' (tx,ty) (l2x,l2y) (if (mem elx l2x && mem ely l2y) then (elx::accx,ely::accy) 
          else (if (mem elx l2x) then (elx::accx,accy) 
          else (if (mem ely l2y) then (accx,ely::accy) 
          else (accx,accy))))
  in
  inter' l1 l2 ([],[])
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

let nodeComponentsX l =
  let rec nodeComponentsX' l acc = match l with
    | [] -> acc
    | (lx,ly)::t -> nodeComponentsX' t (lx@acc)
  in
  nodeComponentsX' l []
;;

let nodeComponentsY l =
  let rec nodeComponentsY' l acc = match l with
    | [] -> acc
    | (lx,ly)::t -> nodeComponentsY' t (ly@acc)
  in
  nodeComponentsY' l []
;;

let strongComponents g matchingEdges =
  let rec strongComponents' (Graph(x,y,_) as g) matchingEdges components =
    match filter (fun el -> (not (mem el (nodeComponentsX components)))) x with
    | [] -> (match filter (fun el -> (not (mem el (nodeComponentsY components)))) y with
      | [] -> components
      | el::t -> strongComponents' g matchingEdges ((inter (dfsY el g matchingEdges) (revDfsY el g matchingEdges))::components))
    | el::t -> strongComponents' g matchingEdges ((inter (dfsX el g matchingEdges) (revDfsX el g matchingEdges))::components)
  in
  strongComponents' g matchingEdges []
;;

let edgesNode n l =
  let rec edgesNode' n l edges = match l with
    | [] -> edges
    | el::t -> edgesNode' n t ((n,el)::edges)
  in
  edgesNode' n l []
;;

let componentEdges component g =
  let rec componentEdges' component g edges =
    match component with
    | ([],_) -> edges
    | (_,[]) -> edges
    | (e::t,l2) -> let l = filter (fun el -> (mem el (neighborsX g e))) l2 in
        componentEdges' (t,l2) g ((edgesNode e l)@edges)
  in
  componentEdges' component g []
;;


let strongComponentsEdges g components =
  let rec strongComponentsEdges' g components edges =
    match components with
    | [] -> edges
    | el::t -> strongComponentsEdges' g t ((componentEdges el g)@edges)
  in
  strongComponentsEdges' g components []
;;




let rec filterEdges' (Graph(x,y,e)) matchingEdges used (re,ve) =
  match e with
  | [] -> (re,ve)
  | (Edge(elx,ely))::t -> filterEdges' (Graph(x,y,t)) matchingEdges used
    (if (not (mem (elx,ely) used)) then (if (mem (elx,ely) matchingEdges) then (re,(elx,ely)::ve) else ((elx,ely)::re,ve) ) else (re,ve))
;;



let filterEdges (Graph(x,y,e) as g) matchingEdges = 
  let (mnX, mnY) = split matchingEdges in
  let used = (dfsUsed (filter (fun el -> (not (mem el (mnY)))) y) g matchingEdges)
      @ (strongComponentsEdges g (strongComponents g matchingEdges)) in
  filterEdges' g matchingEdges used ([],[])
;;

let allDiff g = let (mn, me) = matching g in
  let meMax = matchingMax g me in
  filterEdges g meMax
;;



