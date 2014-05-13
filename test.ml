open Alldiff
open Printf
open List



let rec printn_list = function
        | [] -> ()
        | (Node(e))::l -> print_int e; print_string " "; printn_list l
;;

let rec printe_list = function
	| [] -> print_string "\n"
	| (Node(x),Node(y))::l -> printf "(%d,%d)" x y; printe_list l
;;


let graphe1 = Graph([Node(1);Node(2);Node(3);Node(4);Node(5);Node(6)],[Node(7);Node(8);Node(9);Node(10);Node(11);Node(12);Node(13)],[Edge((Node(1),Node(7)));Edge((Node(1),Node(8)));(*Edge((Node(2),Node(8)));*)Edge((Node(2),Node(9)));(*Edge((Node(3),Node(7)));*)Edge((Node(3),Node(9)));Edge((Node(4),Node(8)));Edge((Node(4),Node(10)));Edge((Node(5),Node(9)));Edge((Node(5),Node(10)));Edge((Node(5),Node(11)));Edge((Node(5),Node(12)));Edge((Node(6),Node(12)));Edge((Node(6),Node(13)))])


let graphe2 = Graph([Node(1);Node(2);Node(3);Node(4)],[Node(5);Node(6);Node(7);Node(8)], [Edge((Node(1),Node(5)));Edge((Node(1),Node(6)));Edge((Node(2),Node(5)));Edge((Node(2),Node(6)));Edge((Node(3),Node(6)));Edge((Node(3),Node(7)));Edge((Node(4),Node(7)));Edge((Node(4),Node(8)))])
;;

let (Graph(x3,y3,e3) as graphe3) = Graph([Node(1);Node(2);Node(3)],[Node(4);Node(5);Node(6)],[Edge((Node(1),Node(4)));Edge((Node(1),Node(5)));Edge((Node(1),Node(6)));Edge((Node(2),Node(4)));Edge((Node(2),Node(5)));Edge((Node(3),Node(6)))])
;;


let (mnode1, medge1) = matching graphe1;;
let (mnode2, medge2) = matching graphe2;;
let (mnode3, medge3) = matching graphe3;;
let (mnodex3,mnodey3) = split medge3;;

let alp3 = alternating_path (Node(3)) graphe3 medge3 mnode3;;
let upmatch3 = upgrade_matching medge3 alp3;;


printn_list mnode1;print_string "\n";;
printe_list medge1;print_string "\n";;
printn_list mnode2;print_string "\n";;
printe_list medge2;print_string "\n";;
printn_list mnode3;print_string "\n";;
printe_list medge3;print_string "\n";;
printe_list alp3;;
printe_list (matching_max graphe3 []);;
printe_list upmatch3;;
printe_list (matching_max graphe3 upmatch3);; 
printe_list (matching_max graphe2 []);;
printe_list (matching_max graphe1 []);;                     
