open Alldiff
open Printf
open List

let rec listToNodeX l1 acc = match l1 with
	| [] -> acc
	| el::t -> listToNodeX t (NodeX(el)::acc)
;;

let rec listToNodeY l1 acc = match l1 with
        | [] -> acc
        | el::t -> listToNodeY t (NodeY(el)::acc)
;;

let rec toNode (l1,l2) = (listToNodeX l1 [], listToNodeY l2 [])
;;	 


let rec printn_list = function
        | [] -> ()
        | (NodeX(X(e)))::l -> print_int e; print_string " "; printn_list l
        | (NodeY(Y(e)))::l -> print_int e; print_string " "; printn_list l
;;

let rec printe_list = function
	| [] -> print_string "\n"
	| (X(x),Y(y))::l -> printf "(%d,%d)" x y; printe_list l
;;

let rec printe2_list = function
        | [] -> print_string "\n"
        | (Edge(X(x),Y(y)))::t -> printf "(%d,%d)" x y; printe2_list t
;;

let printn2_list node = match node with
	| (l1,l2) -> print_string "( "; printn_list l1; print_string ", "; printn_list l2; print_string ")"
;;

let rec printComp comp = match comp with 
	| [] -> ()
	| l::tl -> printn2_list (toNode l); printComp tl
;;

let (Graph(x1,y1,e1) as graphe1) = Graph([X(1);X(2);X(3);X(4);X(5);X(6)],[Y(7);Y(8);Y(9);Y(10);Y(11);Y(12);Y(13)],[Edge((X(1),Y(7)));Edge((X(1),Y(8)));Edge((X(2),Y(8)));Edge((X(2),Y(9)));Edge((X(3),Y(7)));Edge((X(3),Y(9)));Edge((X(4),Y(8)));Edge((X(4),Y(10)));Edge((X(5),Y(9)));Edge((X(5),Y(10)));Edge((X(5),Y(11)));Edge((X(5),Y(12)));Edge((X(6),Y(12)));Edge((X(6),Y(13)))])


let (Graph(x2,y2,e2) as graphe2) = Graph([X(1);X(2);X(3);X(4)],[Y(5);Y(6);Y(7);Y(8);Y(9)], [Edge((X(1),Y(5)));Edge((X(1),Y(6)));Edge((X(2),Y(5)));Edge((X(2),Y(6)));Edge((X(3),Y(6)));Edge((X(3),Y(7)));Edge((X(4),Y(7)));Edge((X(4),Y(8)));Edge((X(3),Y(9)))])
;;

let (Graph(x3,y3,e3) as graphe3) = Graph([X(1);X(2);X(3)],[Y(4);Y(5);Y(6)],[Edge((X(1),Y(4)));Edge((X(1),Y(5)));Edge((X(1),Y(6)));Edge((X(2),Y(4)));Edge((X(2),Y(5)));Edge((X(3),Y(6)))])
;;


let (mnode1, medge1) = matching graphe1;;
let (mnode2, medge2) = matching graphe2;;
let (mnode3, medge3) = matching graphe3;;


let (mnodex3,mnodey3) = split medge3;;

printe2_list e1;;
printe2_list e2;;
printe2_list e3;;

let medge1Max = matchingMax graphe1 medge1;;
let medge2Max = matchingMax graphe2 medge2;;
let medge3Max = matchingMax graphe3 medge3;;

printe_list medge1Max;;
printe_list medge2Max;;
printe_list medge3Max;;

let scc1 = strongComponents graphe1 medge1Max;;
let scc2 = strongComponents graphe2 medge2Max;;
let scc3 = strongComponents graphe3 medge3Max;;

printComp scc1;print_string "\n";;
printComp scc2;print_string "\n";;
printComp scc3;print_string "\n";;

let ce1 = strongComponentsEdges graphe1 scc1;;
let ce2 = strongComponentsEdges graphe2 scc2;;
let ce3 = strongComponentsEdges graphe3 scc3;;

let used1 = ((dfsUsed (filter (fun el -> (not (mem el [Y(7);Y(8);Y(9);Y(10);Y(12);Y(13)]))) [Y(7);Y(8);Y(9);Y(10);Y(11);Y(12);Y(13)]) graphe1 medge1Max) @ ce1);;
let used2 = ((dfsUsed (filter (fun el -> (not (mem el [Y(5);Y(6);Y(8);Y(9)]))) [Y(5);Y(6);Y(7);Y(8);Y(9)]) graphe2 medge2Max) @ ce2);;
let used3 = ((dfsUsed (filter (fun el -> (not (mem el [Y(4);Y(5);Y(6)]))) [Y(4);Y(5);Y(6)]) graphe3 medge3Max) @ ce3);;

printe_list used1;;
printe_list used2;;
printe_list used3;;


let (re1,ve1) = allDiff graphe1;;
let (re2,ve2) = allDiff graphe2;;
let (re3,ve3) = allDiff graphe3;;
print_string "\n";;
printe_list re1;;printe_list ve1;;
printe_list re2;;printe_list ve2;;
printe_list re3;;printe_list ve3;;
print_string "\n";;print_string "\n";;
(*
printn_list (listToNodeY (filter (fun el -> (not (mem el [Y(5);Y(6);Y(8);Y(9)]))) [Y(5);Y(6);Y(7);Y(8);Y(9)]) []);;

let used2 = ((dfsUsed (filter (fun el -> (not (mem el [Y(5);Y(6);Y(8);Y(9)]))) [Y(5);Y(6);Y(7);Y(8);Y(9)]) graphe2 medge2Max) @ (strongComponentsEdges graphe2 (strongComponents graphe2 medge2Max)));;

let used1 = ((dfsUsed (filter (fun el -> (not (mem el [Y(7);Y(8);Y(9);Y(10);Y(12);Y(13)]))) [Y(7);Y(8);Y(9);Y(10);Y(11);Y(12);Y(13)]) graphe1 medge1Max) @ (strongComponentsEdges graphe1 (strongComponents graphe1 medge1Max)));;

printe_list used1;;
printe_list used2;;

let e = (X(5),Y(10)) in
  if (not (mem e used1)) then 
    (if (mem e medge1Max) then 
      (printe_list [(X(0),Y(0))]) else 
      (printe_list [e]) ) else 
    (printe_list [(X(0),Y(0))])
;;

let alp3 = alternatingPath (X(3)) graphe3 medge3 mnode3;;
let upmatch3 = upgradeMatching medge3 alp3;;




let testf1 = filter (fun el -> (not (mem el []))) [X(1)];;
let testdfsx1 = dfsX (X(1)) graphe1 medge1Max;; 
let testdfsy1 = dfsY (Y(7)) graphe1 medge1Max;;
let testrevdfsx1 = revDfsX (X(1)) graphe1 medge1Max;;
let testrevdfsx1 = revDfsY (Y(7)) graphe1 medge1Max;;

let (testx,testy) = inter ([X(1);X(2)],[Y(8);Y(9)]) ([X(1);X(3)],[Y(7);Y(8)]);;
(*printn2_list (toNode (testx,testy)); print_string "\n";;*)
(*printn_list mnode1;print_string "\n";;*)
printe_list medge1;print_string "\n";;
(*printn_list mnode2;print_string "\n";;*)
printe_list medge2;print_string "\n";;
(*printn_list mnode3;print_string "\n";;*)
printe_list medge3;print_string "\n";;
printe_list alp3;;
printe_list (matchingMax graphe3 []);;
printe_list upmatch3;;
printe_list (matchingMax graphe3 upmatch3);; 
printe_list medge3Max;print_string "\n";;
printe_list (matchingMax graphe1 []);;
let new_testdfsx1 = toNode testdfsx1;;
printn2_list (toNode testdfsx1);print_string "\n";;
printComp scc3;print_string "\n";;
printn2_list (toNode (dfsX (X(1)) graphe3 medge3Max)); print_string "\n";;
printn2_list (toNode (revDfsX (X(1)) graphe3 medge3Max)); print_string "\n";;
let y = yMatching (X(2)) medge3Max;;
printn_list (listToNodeY [y] []);;
let newStack = (filter (fun el -> (not (mem el ([]@[])))) (neighborsY graphe3 (Y(6))))@[];;
printn_list (listToNodeX newStack []);;
printn2_list (toNode (dfsY (Y(4)) graphe3 medge3Max)); print_string "\n";;
printn2_list (toNode (dfsY (Y(5)) graphe3 medge3Max)); print_string "\n";;
printn2_list (toNode (dfsY (Y(6)) graphe3 medge3Max)); print_string "\n";;
printn2_list (toNode (dfsY (Y(7)) graphe2 medge2Max)); print_string "\n";;
printe_list (dfsEdges (Y(7)) graphe2 medge2Max);;
printn_list (listToNodeX (neighborsY graphe3 (Y(11))) []);;
*)
