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
(*let neighborsX g n =
  let rec neighborsX' n (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> neighborsX' n (Graph (x,y,t)) (if i = n then (j::ns) else ns)
  in
  neighborsX' n g []
;;
*)
(* demote : edge list -> (nodeX, nodeY) list *)
let demote es = map (fun (Edge(n,m)) -> (n,m)) es
;;
(* promote : (nodeX, nodeY) list -> edge list *)
let promote es = map (fun ((n,m)) -> Edge(n,m)) es
;;

let neighborsX (Graph(_,_,e)) n = map snd (filter (fun (u,v) -> u = n) (demote e))
;;

let neighborsY (Graph(_,_,e)) n = map fst (filter (fun (u,v) -> v = n) (demote e))
;;

(* neighbors_y : node -> graph -> node list 
 * neighbors_y y g prend le sommet y de Y et renvoie la liste de ses voisins 
 * dans g(X,Y,E) 
 * neighbor_y' : node -> graph -> node list -> node list *)
(*let neighborsY g n =
  let rec neighborsY' n (Graph(x,y,e)) ns =
    match e with
    | [] -> ns
    | Edge(i,j)::t -> neighborsY' n (Graph (x,y,t)) (if j = n then (i::ns) else ns)
  in
  neighborsY' n g []
;;
*)

(*
(* remove_edge : node -> edge list -> edge list
 * remove_edge n es enlève l'arrete (n,y) et l'arrette (x,y) de la liste es *)
let rec removeEdge n es = match es with
  | [] -> []
  | (x,y)::es -> if n = x then removeEdge y es else 
    if n = y then es else removeEdge n es
;;
*)

(* x_matching : node -> edge list -> node
 * x_matching y es renvoie le sommet x couplé à l'arrette y dans la liste es *)
let matched (Matching(es)) n = 
  match n with
  | NodeX(x) -> NodeY(snd (find (fun (u,v) -> u = x) (demote es)))
  | NodeY(y) -> NodeX(fst (find (fun (u,v) -> v = y) (demote es)))
;;


  
(* remove : node -> node list -> node
 * remove e l  renvoie la liste l privée l'élément e *)
let remove e l = filter (fun x -> x != e) l
;;

let (|--) baseList removalList = filter (fun x -> not (mem x removalList)) baseList;;

let (|^|) l1 l2 = filter (fun el -> (mem el l1)) l2
;;
(* upgrade_matching : edge list -> edge list -> edge list
 * upgrade_matching m path *) 
let augment (Matching(es)) path = Matching((es |-- (es |^| path)) @ (path |-- (es |^| path)))
;;

(*
let rec edgeList x l edgeUsed = match l with
  | [] -> edgeUsed
  | e::t -> if (mem (e,x) edgeUsed) then edgeList x t edgeUsed else edgeList x t ((e,x)::edgeUsed)
;;
*)
(*

let rec dfsEdges' root g m stack (xNodes, yNodes) edges =
  let explored = stack@xNodes in
    let stack' = (filter (fun x -> (not (mem x explored))) (neighborsY g root))@stack in
      match stack' with
      | [] -> edges
      | x::t -> let y = matched m (NodeX(x)) in
          dfsEdges' y g m t (x::xNodes, y::yNodes) ((x, y)::(n, root)::edges)
;;*)
(*
let dfsEdgesX x g m = 
    let y' = matched x m in
    dfsEdges' y' g m [] ([x], [y']) [(x, y')]
;;

let rec dfsEdgesl xs g m edges =
  match xs with
  | [] -> edges
  | x::t -> let edges' = dfsEdgesX x g m in
      dfsEdgesl t g m (edges'@edges)
;;

let dfsEdges y g m = let ny = neighborsY g y in
  dfsEdgesl ny g m (edgeList y ny [])
;;

let dfsUsed l g m =
  let rec dfsUsed' l g m edges = match l with
    | [] -> edges
    | y::t -> dfsUsed' t g m ((dfsEdges y g m)@edges)
  in
  dfsUsed' l g m []
;;

let rec dfs' y g m stack (visitedX, visitedY) =
  try
    let notX = xMatching y m in
    let newStack = (filter (fun el -> (not (mem el (notX::stack@visitedX)))) (neighborsY g y))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> let newY = yMatching el m in
          dfs' newY g m t 
          (if (mem newY visitedY) then (el::visitedX, visitedY) 
          else (el::visitedX, newY::visitedY))
  with
    Not_found -> (visitedX, visitedY)
;;

let rec revDfs' x g m stack (visitedX, visitedY) =
  try
    let notY = yMatching x m in
    let newStack = (filter (fun el -> (not (mem el (notY::stack@visitedY)))) (neighborsX g x))@stack in
      match newStack with
      | [] -> (visitedX, visitedY)
      | el::t -> let newX = xMatching el m in
          revDfs' newX g m t 
          (if (mem newX visitedX) then (visitedX, el::visitedY) 
          else (newX::visitedX, el::visitedY))
  with
    Not_found -> (visitedX, visitedY)
;;



let dfsY y g m = dfs' y g m [] ([], [y])
;;

let revDfsX x g m = revDfs' x g m [] ([x], [])
;;

let dfsX x g m = try
    let newY = yMatching x m in
    dfs' newY g m [] ([x], [newY])
  with
    Not_found -> ([x], [])
;;

let revDfsY y g m = try
    let newX = xMatching y m in
    revDfs' newX g m [] ([newX], [y])
  with
    Not_found -> ([], [y])
;;
*)

let intersection (l1x,l1y) (l2x,l2y) = ( l1x |^| l2x, l1y |^| l2y)
;;



(* matching : graph -> (node list * edge list)
 * matching g renvoie un couplage du graphe g *)
let matching g =
  let rec matching' (Graph(x,y,e) as g) (Matching(es) as m) =
    match x with
    | [] -> m
    | elx::t -> 
      try
        let (xNodes,yNodes) = split (demote es) in
        let ely = find (fun el -> not (mem el yNodes)) (neighborsX g elx) in
        matching' (Graph(t,y,e)) (Matching((Edge(elx,ely))::es)) 
      with
        Not_found -> matching' (Graph(t,y,e)) m
  in
  matching' g (Matching([]))
;;

(* augmentingPath : graph -> matching -> nodeX -> edge list *)
let augmentingPath g m x =
  let rec augmentingPath' g (Matching(e) as m) x visited =
    match filter (fun el -> (not (mem el visited))) (neighborsX g x) with
    | [] -> [] 
    | y::t -> 
      let (xNodes,yNodes) = split (demote e) in
      if (mem y yNodes)
        then let (NodeX(x')) = matched m (NodeY(y)) in
          try
            (Edge(x',y))::(Edge(x,y))::(augmentingPath' g m x' (y::visited))
          with
            Not_found -> augmentingPath' g m x' (y::visited)
        else (Edge(x,y))::[]
  in
  augmentingPath' g m x []
;;

(* matching_max : graph -> edge list -> edge list
 * matching_max g m renvoit le couplage max du graphe g *)
let maximumMatching (Graph(x,_,_) as g) (Matching(es) as m) =
  let (xNodes, yNodes) = split (demote es) in 
    let l = filter (fun el -> (not (mem el xNodes))) x in
      let rec maximumMatching' m l =
        match l with
        | [] -> m
        | el::xs -> try 
            let path = augmentingPath g m el in 
              maximumMatching' (augment m path) xs
            with
              Not_found -> maximumMatching' m xs
      in
      maximumMatching' m l
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
(*
let strongComponents g m =
  let rec strongComponents' (Graph(x,y,_) as g) m components =
    match filter (fun el -> (not (mem el (nodeComponentsX components)))) x with
    | [] -> (match filter (fun el -> (not (mem el (nodeComponentsY components)))) y with
      | [] -> components
      | el::t -> strongComponents' g m ((intersection (dfsY el g m) (revDfsY el g m))::components))
    | el::t -> strongComponents' g m ((intersection (dfsX el g m) (revDfsX el g m))::components)
  in
  strongComponents' g m []
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




let rec filterEdges' (Graph(x,y,e)) m used (re,ve) =
  match e with
  | [] -> (re,ve)
  | (Edge(elx,ely))::t -> filterEdges' (Graph(x,y,t)) m used
    (if (not (mem (elx,ely) used)) then (if (mem (elx,ely) m) then (re,(elx,ely)::ve) else ((elx,ely)::re,ve) ) else (re,ve))
;;



let filterEdges (Graph(x,y,e) as g) m = 
  let (xNodes, yNodes) = split m in
  let used = (dfsUsed (filter (fun el -> (not (mem el (yNodes)))) y) g m)
      @ (strongComponentsEdges g (strongComponents g m)) in
  filterEdges' g m used ([],[])
;;

exception X_not_matched;;

let allDiff (Graph(x,_,_) as g) = let (mn, me) = matching g in
  let meMax = maximumMatching g me in
  if (length meMax < length x)
  then raise X_not_matched
  else filterEdges g meMax
;;


*)
