
open List



type nodeX = X of int;;
type nodeY = Y of int;;
type node = NodeX of nodeX | NodeY of nodeY;;
type edge = Edge of nodeX * nodeY;;
type graph = Graph of nodeX list * nodeY list * edge list;;
type matching = Matching of edge list;;



(* demote : edge list -> (nodeX, nodeY) list *)
let demote es = map (fun (Edge(n,m)) -> (n,m)) es
;;

(* promote : (nodeX, nodeY) list -> edge list *)
let promote es = map (fun (n,m) -> Edge(n,m)) es
;;

let splitMatching (Matching(es)) = split (demote es)
;;

let neighborsInY (Graph(_,_,e)) n = map snd (filter (fun (u,v) -> u = n) (demote e))
;;

let neighborsInX (Graph(_,_,e)) n = map fst (filter (fun (u,v) -> v = n) (demote e))
;;

(* matched : matching -> node -> node
 * matched renvoie le sommet couplé s'il existe *)
let matched (Matching(es)) n = 
  match n with
  | NodeX(x) -> NodeY(snd (find (fun (u,v) -> u = x) (demote es)))
  | NodeY(y) -> NodeX(fst (find (fun (u,v) -> v = y) (demote es)))
;;

let add (Matching(es)) e = Matching(e::es) 
;;

(* (|--) : 'a list -> 'a list -> 'a list 
 * a |-- b est la différence de a par b *)
let (|--) baseList removalList = filter (fun x -> not (mem x removalList)) baseList;;

(* (|^|) : 'a list -> 'a list -> 'a list 
 * a |^| b est l'intersection de a et b *)
let (|^|) l1 l2 = filter (fun el -> (mem el l1)) l2
;;

(* symDiff : 'a list -> 'a list -> 'a list
 * symDiff renvoie la différence symétrique de a et b *) 
let symDiff a b = (a |-- b) @ (b |-- a )
;;

(* augment : matching -> edge list -> matching 
 * augment renvoie le couplage correspondant à la différence symétrique
 * entre le couplage et la chaîne donnés ce qui correspond à un couplage 
 * augmenté dans le cas d'une chaîne augmentante *)
let augment (Matching(es)) path = Matching(symDiff es path)
;;

(* findAvailableEdge : graph -> nodeX -> matching -> matching 
 * findAvailableEdge renvoie le couplage augmenté d'une nouvelle arête si elle existe *)
let findAvailableEdge g x m =
  try 
    let (_, matchedYs) = splitMatching m in
    let y = find (fun n -> not (mem n matchedYs)) (neighborsInY g x) in
      add m (Edge(x,y))
  with
    Not_found -> m
;;

(* matching : graph -> matching
 * matching renvoie un couplage du graphe *)
let matching g =
  let matching' (Graph(xs,_,_) as g) m =
    fold_right (findAvailableEdge g) xs m
  in
  matching' g (Matching([]))
;;

(* augmentingPath : graph -> matching -> nodeX -> edge list
 * augmentingPath renvoie une chaîne augmentante partant du noeud donné si elle existe *)
let augmentingPath g m root =
  let rec augmentingPath' g m root visited =
    match (neighborsInY g root) |-- visited with
    | [] -> [] 
    | y::t -> 
      let (_, yNodes) = splitMatching m in
      if (mem y yNodes)
        then let (NodeX(x)) = matched m (NodeY(y)) in
          try
            (Edge(x,y))::(Edge(root,y))::(augmentingPath' g m x (y::visited))
          with
            Not_found -> augmentingPath' g m x (y::visited)
        else (Edge(root,y))::[]
  in
  augmentingPath' g m root []
;;

(* useAugmentingPath : graph -> nodeX -> matching -> matching
 * useAugmentingPath renvoie le couplage augmenté d'une chaîne augmentante si elle existe *)
let useAugmentingPath g x m = augment m (augmentingPath g m x)
;;

(* maximumMatching : graph -> matching -> matching
 * maximumMatching transforme un couplage donné en un couplage de cardinal maximum *)
let maximumMatching (Graph(xs,_,_) as g) m =
  let unsaturatedXs =  xs |-- (fst (splitMatching m)) in
  fold_right (useAugmentingPath g) unsaturatedXs m 
;;

let cut g n = 
  match n with
  | NodeX(x) -> map (fun y -> Edge(x,y)) (neighborsInY g x)
  | NodeY(y) -> map (fun x -> Edge(x,y)) (neighborsInX g y)
;;

(* dfsEdges' : nodeY -> graph -> matching -> nodeX list -> nodeX list -> edges list -> edges list *)
let rec dfsEdges' (NodeY(r) as root) g m stack visitedXs edges =
  let seen = stack @ visitedXs in
  let stack' = ((neighborsInX g r) |-- seen) @ stack in
    match stack' with
    | [] -> (cut g root) @ edges
    | x::xs -> let y = matched m (NodeX(x)) in
        dfsEdges' y g m xs (x::visitedXs) ((cut g root) @ edges)
;;

(*
(* dfsEdgesX : nodeX -> graph -> matching -> edges list *)
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

let dfsEdges y g m = let ny = neighborsInX g y in
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
    let newStack = (filter (fun el -> (not (mem el (notX::stack@visitedX)))) (neighborsInX g y))@stack in
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
    let newStack = (filter (fun el -> (not (mem el (notY::stack@visitedY)))) (neighborsInY g x))@stack in
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
    | (e::t,l2) -> let l = filter (fun el -> (mem el (neighborsInY g e))) l2 in
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
