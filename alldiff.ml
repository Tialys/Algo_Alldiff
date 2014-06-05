
open List



type nodeX = X of int;;
type nodeY = Y of int;;
type node = NodeX of nodeX | NodeY of nodeY;;
type edge = nodeX * nodeY;;
type graph = Graph of nodeX list * nodeY list * edge list;;
type matching = Matching of edge list;;


(* splitMatching : matching -> (nodeX list * nodeY list) *)
let splitMatching (Matching(es)) = split es
;;

(* neighborsInX : graph -> nodeY -> nodeX list *)
let neighborsInX (Graph(_,_,e)) y = map fst (filter (fun (u,v) -> v = y) e)
;;

(* neighborsInY : graph -> nodeX -> nodeY list *)
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
let (|--) baseList removalList = filter (fun x -> not (mem x removalList)) baseList
;;

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
        then (match matched m (NodeY(y)) with
             | NodeX(x) ->
          (try
            (x,y)::(root,y)::(augmentingPath' g m x (y::visited))
          with
            Not_found -> augmentingPath' g m root (y::visited))
             | _ -> raise Not_found )
          else (root,y)::[]
  in
  augmentingPath' g m root []
;;

(* useAugmentingPath : graph -> nodeX -> matching -> matching
 * useAugmentingPath renvoie le couplage augmenté d'une chaîne augmentante si elle existe *)
let useAugmentingPath g x m = 
  try
    augment m (augmentingPath g m x)
  with
  Not_found -> augment m []
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
 * dfsEdges' est une fonction auxilière. Elle renvoie la liste des arêtes atteignables 
 * par un parcours en profondeur partant du sommet donné. Ce sommet sera forcement de type nodeY.
 * Le graphe est orienté : de X vers Y pour les arêtes dans le couplage, de Y vers X pour les autres. *)
let rec dfsEdges' g m (NodeY(r) as root) stack visitedXs edges =
  let seen = stack @ visitedXs in
  let stack' = ((neighborsInX g r) |-- seen) @ stack in
    match stack' with
    | [] -> (cut g root) @ edges
    | x::xs -> let y = matched m (NodeX(x)) in
        dfsEdges' g m y xs (x::visitedXs) ((cut g root) @ edges)
;;

(* dfsEdges : graph -> matching -> node -> edge list 
 * dfsEdges renvoie la liste des arêtes atteignable par un parcours en profondeur partant du sommet donné. 
 * Si le sommet est dans X, alors il est saturé et on lance dfsEdges' avec le sommet y couplé,
 * en ajoutant (x,y) dans les arêtes parcourus et x dans les sommets visité.
 * Si le sommet est dans Y, si il est saturé *)
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
    | l::ls -> uniqs' (uniq (map (fun uniqList -> uniqList |-- l) ls)) (l::aux)
  in
  uniqs' baseList  []
;;

let hasMoreThanOne l =
  match l with
  | [] -> false
  | [e] -> false
  | _ -> true
;;

let rec removeEmpty ll = filter hasMoreThanOne ll
;; 

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



