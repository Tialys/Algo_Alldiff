
open List



type nodeX = X of int;;
type nodeY = Y of int;;
type node = NodeX of nodeX | NodeY of nodeY;;
type edge = nodeX * nodeY;;
type graph = Graph of nodeX list * nodeY list * edge list;;
type matching = Matching of edge list;;


<<<<<<< HEAD
=======
let nodey ys = map (fun y -> NodeY(y)) ys
;;

>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
(* splitMatching : matching -> (nodeX list * nodeY list) *)
let splitMatching (Matching(es)) = split es
;;

<<<<<<< HEAD
(* neighborsInX : graph -> nodeY -> nodeX list *)
let neighborsInX (Graph(_,_,e)) y = map fst (filter (fun (u,v) -> v = y) e)
;;

(* neighborsInY : graph -> nodeX -> nodeY list *)
=======
(* neighbors graph -> node -> node list *)
let neighborsInX (Graph(_,_,e)) y = map fst (filter (fun (u,v) -> v = y) e)
;;

>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
let neighborsInY (Graph(_,_,e)) x = map snd (filter (fun (u,v) -> u = x) e)
;;


(* matched : matching -> node -> node
 * matched renvoie le sommet couplé s'il existe *)
let matched (Matching(es)) n = 
  match n with
  | NodeX(x) -> NodeY(snd (find (fun (u,v) -> u = x) es))
  | NodeY(y) -> NodeX(fst (find (fun (u,v) -> v = y) es))
;;

(* add : matching -> edge -> matching *)
let add (Matching(es)) e = Matching(e::es) 
;;

(* (|--) : 'a list -> 'a list -> 'a list 
 * a |-- b est la différence de a par b *)
<<<<<<< HEAD
let (|--) baseList removalList = filter (fun x -> not (mem x removalList)) baseList
;;
=======
let (|--) baseList removalList = filter (fun x -> not (mem x removalList)) baseList;;
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8

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
      add m (x,y)
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
  let (_, yNodes) = splitMatching m in
    match (neighborsInY g root) |-- visited with
    | [] -> raise Not_found
    | y::t -> 
        if (mem y yNodes)
<<<<<<< HEAD
        then (match matched m (NodeY(y)) with
             | NodeX(x) ->
          (try
            (x,y)::(root,y)::(augmentingPath' g m x (y::visited))
          with
            Not_found -> augmentingPath' g m root (y::visited))
             | _ -> raise Not_found )
          else (root,y)::[]
=======
        then let (NodeX(x)) = matched m (NodeY(y)) in
          try
            (x,y)::(root,y)::(augmentingPath' g m x (y::visited))
          with
            Not_found -> augmentingPath' g m root (y::visited)
        else (root,y)::[]
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
  in
  augmentingPath' g m root []
;;

(* useAugmentingPath : graph -> nodeX -> matching -> matching
 * useAugmentingPath renvoie le couplage augmenté d'une chaîne augmentante si elle existe *)
<<<<<<< HEAD
let useAugmentingPath g x m = 
  try
    augment m (augmentingPath g m x)
  with
  Not_found -> augment m []
=======
let useAugmentingPath g x m = augment m (augmentingPath g m x)
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
;;

(* maximumMatching : graph -> matching -> matching
 * maximumMatching transforme un couplage donné en un couplage de cardinal maximum *)
let maximumMatching (Graph(xs,_,_) as g) m =
  let unsaturatedXs =  xs |-- (fst (splitMatching m)) in
  fold_right (useAugmentingPath g) unsaturatedXs m 
;;

(* cut : graph -> node -> edge list 
 * cut renvoie la coupe du sommet donné *)
let cut g n = 
  match n with
  | NodeX(x) -> map (fun y -> (x,y)) (neighborsInY g x)
  | NodeY(y) -> map (fun x -> (x,y)) (neighborsInX g y)
;;

(* dfsEdges' : nodeY -> graph -> matching -> nodeX list -> nodeX list -> edges list -> edges list 
<<<<<<< HEAD
 * dfsEdges' est une fonction auxilière. Elle renvoie la liste des arêtes atteignables 
 * par un parcours en profondeur partant du sommet donné. Ce sommet sera forcement de type nodeY.
 * Le graphe est orienté : de X vers Y pour les arêtes dans le couplage, de Y vers X pour les autres. *)
=======
 * dfsEdges' est une fonction auxilière *)
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
let rec dfsEdges' g m (NodeY(r) as root) stack visitedXs edges =
  let seen = stack @ visitedXs in
  let stack' = ((neighborsInX g r) |-- seen) @ stack in
    match stack' with
    | [] -> (cut g root) @ edges
    | x::xs -> let y = matched m (NodeX(x)) in
        dfsEdges' g m y xs (x::visitedXs) ((cut g root) @ edges)
;;

<<<<<<< HEAD
(* dfsEdges : graph -> matching -> node -> edge list 
 * dfsEdges renvoie la liste des arêtes atteignable par un parcours en profondeur partant du sommet donné. 
 * Si le sommet est dans X, alors il est saturé et on lance dfsEdges' avec le sommet y couplé,
 * en ajoutant (x,y) dans les arêtes parcourus et x dans les sommets visité.
 * Si le sommet est dans Y, si il est saturé *)
=======
(* unsaturedYDfs : graph -> matching -> nodeY -> edge list 
 * unsaturedYDfs renvoie la liste des arêtes atteint par le parcours partant du sommet donné 
 * avec l'orientation donnée par le couplage donné *)
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
let dfsEdges g m n =
  match n with
  | NodeX(x) -> let (NodeY(y)) = matched m (NodeX(x)) in
      dfsEdges' g m (NodeY(y)) [] [x] [(x,y)]
  | NodeY(y) -> 
    try
      let (NodeX(x)) = matched m (NodeY(y)) in
      dfsEdges' g m (NodeY(y)) [] [x] [(x,y)]
    with
      Not_found -> dfsEdges' g m (NodeY(y)) [] [] []
;;


(* revDfsEdges' : graph -> matching -> nodeX -> nodeY list -> nodeY list -> edge list -> edge list *)
let rec revDfsEdges' g m (NodeX(r) as root) stack visitedYs edges =
  let seen = stack @ visitedYs in
  let stack' = ((neighborsInY g r) |-- seen) @ stack in
    match stack' with
    | [] -> (cut g root) @ edges
    | y::ys -> 
      try
        let x = matched m (NodeY(y)) in
        revDfsEdges' g m x ys (y::visitedYs) ((cut g root) @ edges)
      with
        Not_found -> edges
;;

(* revDfsEdges : graph -> matching -> node -> edge list *)
let revDfsEdges g m n =
  match n with
  | NodeX(x) -> revDfsEdges' g m (NodeX(x)) [] [] []
  | NodeY(y) -> 
      try
        let (NodeX(x)) = matched m (NodeY(y)) in
        revDfsEdges' g m (NodeX(x)) [] [y] [(x,y)]
      with
        Not_found -> []
;;

(* used : graph -> matching -> edge list *)
let used (Graph(_,ys,_) as g) m = 
  let unsaturatedYs = ys |-- (snd (splitMatching m)) in
  map (dfsEdges g m) (map (fun y -> NodeY(y)) unsaturatedYs) 
;;

(* uniq : 'a list -> 'a list 
 * renvoie la liste sans doublon *)
let uniq baseList = fold_right 
  (fun x uniqList -> if (mem x uniqList) then uniqList else (x::uniqList)) 
  baseList []
;;

(* uniqs : 'a list list -> 'a list list
 * renvoie la liste de liste sans doublon (l'ordre dans la liste n'est pas important) *)
let uniqs baseList =
  let rec uniqs' baseList aux = 
    match baseList with 
    | [] -> aux 
<<<<<<< HEAD
    | l::ls -> uniqs' (uniq (map (fun uniqList -> uniqList |-- l) ls)) (l::aux)
=======
    | l::ls -> uniqs' (uniq (map (fun uniqList -> (|--) l uniqList) ls)) (l::aux)
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
  in
  uniqs' baseList  []
;;

<<<<<<< HEAD
let hasMoreThanOne l =
  match l with
  | [] -> false
  | [e] -> false
  | _ -> true
;;

let rec removeEmpty ll = filter hasMoreThanOne ll
;; 

=======
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
(* memEdges : node -> edge list -> bool *)
let memEdges n es = 
  match n with
  | NodeX(x) -> mem x (fst (split es))
  | NodeY(y) -> mem y (snd (split es))
;;

(* strongComponent : graph -> matching -> node -> edge list *)
let strongComponent g m n = uniq ((dfsEdges g m n) |^| (revDfsEdges g m n))
;;

(* strongComponents : graph -> matching -> edge list *)
let strongComponents (Graph(xs,ys,_) as g) m =
  let ns = (map (fun x -> NodeX(x)) xs) @ (map (fun y -> NodeY(y)) ys) in 
<<<<<<< HEAD
  removeEmpty (uniqs (map (strongComponent g m) ns))
;;


let filterEdges (Graph(_,ys,es) as g) (Matching(me) as m) = 
  let (_, yMatched) = splitMatching m in
  let yNodes = map (fun y -> NodeY(y)) (ys |-- yMatched) in
  let used = concat ((map (dfsEdges g m) yNodes) @ (strongComponents g m)) in
  let notUsed = es |-- used in
  partition (fun e -> not (mem e me)) notUsed
;;

exception X_not_matched;;

let allDiff (Graph(x,_,_) as g) = 
  let (Matching(me)) = matching g in
  let (Matching(meMax)) = maximumMatching g (Matching(me)) in
  if (length meMax < length x)
  then raise X_not_matched
  else filterEdges g (Matching(meMax))
;;

let vital' ve es = filter (fun e -> (not (fst e = fst ve)) || (snd e = snd ve)) es
;;

let vital ves es = fold_right (vital') ves es
;;

let diffPropagation (re,ve)  (Graph(xs,ys,es)) = Graph(xs,ys,(vital ve es) |-- re)
;;


let allDiffs gs =
  let rec allDiffs' gs acc = match gs with
    | [] -> acc
    | g::t -> let (re,ve) = allDiff g in
       allDiffs' (map (diffPropagation (re,ve)) t) (map (diffPropagation (re,ve)) (g::acc))
  in
  allDiffs' gs []
;;

let rec propaAllDiffs gs = let gs' = allDiffs gs in
  match gs |-- gs' with
  | [] -> gs'
  | _ -> propaAllDiffs gs'
;;



=======
  uniqs (map (strongComponent g m) ns)
;;

let intersection (l1x,l1y) (l2x,l2y) = ( l1x |^| l2x, l1y |^| l2y)
;;


let nodeComponents l =
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
>>>>>>> 741f7af9987c0ece868970ffd0ec9c94b262c9f8
