open Alldiff
open Printf



let rec printn_list = function
        | [] -> ()
        | (Node(e))::l -> print_int e; print_string " "; printn_list l
;;

let rec printe_list = function
	| [] -> print_string "\n"
	| (Node(x),Node(y))::l -> printf "(%d,%d)" x y; printe_list l
;;


let graphe1 = Graph([Node(1);Node(2);Node(3);Node(4);Node(5);Node(6)],[Node(7);Node(8);Node(9);Node(10);Node(11);Node(12);Node(13)],[Edge((Node(1),Node(7)));Edge((Node(1),Node(8)));Edge((Node(2),Node(8)));Edge((Node(2),Node(9)));Edge((Node(3),Node(7)));Edge((Node(3),Node(9)));Edge((Node(4),Node(8)));Edge((Node(4),Node(10)));Edge((Node(5),Node(9)));Edge((Node(5),Node(10)));Edge((Node(5),Node(11)));Edge((Node(5),Node(12)));Edge((Node(6),Node(12)));Edge((Node(6),Node(13)))])


let graphe2 = Graph([Node(1);Node(2);Node(3);Node(4)],[Node(1);Node(2);Node(3);Node(4)], [Edge((Node(1),Node(1)));Edge((Node(1),Node(2)));Edge((Node(2),Node(1)));Edge((Node(2),Node(2)));Edge((Node(3),Node(2)));Edge((Node(3),Node(3)));Edge((Node(4),Node(3)));Edge((Node(4),Node(4)))])
;;

let graphe3 = Graph([Node(1);Node(2);Node(3)],[Node(4);Node(5);Node(6)],[Edge((Node(1),Node(4)));Edge((Node(1),Node(5)));Edge((Node(1),Node(6)));Edge((Node(2),Node(4)));Edge((Node(2),Node(6)));Edge((Node(3),Node(6)))])
;;


let (mnode1, medge1) = matching graphe1;;

printn_list mnode1;print_string "\n";;
printe_list medge1;print_string "\n";;




                        
