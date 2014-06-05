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

let printeg (Graph(_,_,e)) = printe_list e;; 

let printm (Matching(es)) = printe_list es
;;

let printn2_list node = match node with
	| (l1,l2) -> print_string "( "; printn_list l1; print_string ", "; printn_list l2; print_string ")"
;;

let rec printComp comp = match comp with 
	| [] -> ()
	| l::tl -> printe_list l; printComp tl
;;

let (Graph(x1,y1,e1) as graphe1) = Graph([X(1);X(2);X(3);X(4);X(5);X(6)],[Y(1);Y(2);Y(3);Y(4);Y(5);Y(6);Y(7)],[(X(1),Y(1));(X(1),Y(2));(X(2),Y(2));(X(2),Y(3));(X(3),Y(1));(X(3),Y(3));(X(4),Y(2));(X(4),Y(4));(X(5),Y(3));(X(5),Y(4));(X(5),Y(5));(X(5),Y(6));(X(6),Y(6));(X(6),Y(7))])


let (Graph(x2,y2,e2) as graphe2) = Graph([X(1);X(2);X(3);X(4)],[Y(5);Y(6);Y(7);Y(8);Y(9)], [(X(1),Y(1));(X(1),Y(2));(X(2),Y(1));(X(2),Y(2));(X(3),Y(2));(X(3),Y(3));(X(4),Y(3));(X(4),Y(4));(X(3),Y(5))])
;;

let (Graph(x3,y3,e3) as graphe3) = Graph([X(1);X(2);X(3)],[Y(4);Y(5);Y(6)],[(X(1),Y(1));(X(1),Y(2));(X(1),Y(3));(X(2),Y(1));(X(2),Y(3));(X(3),Y(2))])
;;


let m1 = matching graphe1;;
let m2 = matching graphe2;;
let m3 = matching graphe3;;

let medge1Max = maximumMatching graphe1 m1;;
let medge2Max = maximumMatching graphe2 m2;;
let medge3Max = maximumMatching graphe3 m3;;

printm medge1Max;;
printm medge2Max;;
printm medge3Max;;

let scc1 = strongComponents graphe1 medge1Max;;
let scc2 = strongComponents graphe2 medge2Max;;
let scc3 = strongComponents graphe3 medge3Max;;

let dfs3 = dfsEdges graphe3 medge3Max (NodeX(X(3)));;
let revDfs3 = revDfsEdges graphe3 medge3Max (NodeX(X(3)));;


printe_list dfs3;;
printe_list revDfs3;;
printComp scc1;print_string "\n";;
printComp scc2;print_string "\n";;
printComp scc3;print_string "\n";;


let (re1,ve1) = allDiff graphe1;;
let (re2,ve2) = allDiff graphe2;;
let (re3,ve3) = allDiff graphe3;;
print_string "\n";;
printe_list re1;;printe_list ve1;;
printe_list re2;;printe_list ve2;;
printe_list re3;;printe_list ve3;;
print_string "\n";;print_string "\n";;




let g1 = Graph([X(11);X(12);X(13);X(14)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(11),Y(1));(X(11),Y(2));(X(11),Y(3));(X(11),Y(4));
 (X(12),Y(1));(X(12),Y(2));(X(12),Y(3));(X(12),Y(4));
 (X(13),Y(1));(X(13),Y(2));(X(13),Y(3));(X(13),Y(4));
 (X(14),Y(1));(X(14),Y(2));(X(14),Y(3));(X(14),Y(4))])
;;

let g2 = Graph([X(41);X(42);X(43);X(44)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(41),Y(1));(X(41),Y(2));(X(41),Y(3));(X(41),Y(4));
 (X(42),Y(1));(X(42),Y(2));(X(42),Y(3));(X(42),Y(4));
 (X(43),Y(1));(X(43),Y(2));(X(43),Y(3));(X(43),Y(4));
 (X(44),Y(1));(X(44),Y(2));(X(44),Y(3));(X(44),Y(4))])
;;
let g3 = Graph([X(31);X(32);X(33);X(34)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(31),Y(2));
 (X(32),Y(4));
 (X(33),Y(1));(X(33),Y(2));(X(33),Y(3));(X(33),Y(4));
 (X(34),Y(1))])
;;

let g4 = Graph([X(21);X(22);X(23);X(24)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(21),Y(1));
 (X(22),Y(1));(X(22),Y(2));(X(22),Y(3));(X(22),Y(4));
 (X(23),Y(4));
 (X(24),Y(2))])
;;

let g5 = Graph([X(33);X(34);X(43);X(44)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(33),Y(1));(X(33),Y(2));(X(33),Y(3));(X(33),Y(4));
 (X(34),Y(1));
 (X(43),Y(1));(X(43),Y(2));(X(43),Y(3));(X(43),Y(4));
 (X(44),Y(1));(X(44),Y(2));(X(44),Y(3));(X(44),Y(4))])
;;

let g6 = Graph([X(31);X(32);X(41);X(42)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(31),Y(2));
 (X(32),Y(4));
 (X(41),Y(1));(X(41),Y(2));(X(41),Y(3));(X(41),Y(4));
 (X(42),Y(1));(X(42),Y(2));(X(42),Y(3));(X(42),Y(4))])
;;

let g7 = Graph([X(13);X(14);X(23);X(24)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(13),Y(1));(X(13),Y(2));(X(13),Y(3));(X(13),Y(4));
 (X(14),Y(1));(X(14),Y(2));(X(14),Y(3));(X(14),Y(4));
 (X(23),Y(4));
 (X(24),Y(2))])
;;


let g8 = Graph([X(11);X(12);X(22);X(21)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(11),Y(1));(X(11),Y(2));(X(11),Y(3));(X(11),Y(4));
 (X(12),Y(1));(X(12),Y(2));(X(12),Y(3));(X(12),Y(4));
 (X(22),Y(1));(X(22),Y(2));(X(22),Y(3));(X(22),Y(4));
 (X(21),Y(1))])
;;

let g9 = Graph([X(14);X(24);X(34);X(44)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(14),Y(1));(X(14),Y(2));(X(14),Y(3));(X(14),Y(4));
 (X(24),Y(2));
 (X(34),Y(1));
 (X(44),Y(1));(X(44),Y(2));(X(44),Y(3));(X(44),Y(4))])
;;

let g10 = Graph([X(11);X(21);X(31);X(41)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(11),Y(1));(X(11),Y(2));(X(11),Y(3));(X(11),Y(4));
 (X(21),Y(1));
 (X(31),Y(2));
 (X(41),Y(1));(X(41),Y(2));(X(41),Y(3));(X(41),Y(4))])
;;

let g11 = Graph([X(12);X(22);X(32);X(42)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(12),Y(1));(X(12),Y(2));(X(12),Y(3));(X(12),Y(4));
 (X(22),Y(1));(X(22),Y(2));(X(22),Y(3));(X(22),Y(4));
 (X(32),Y(4));
 (X(42),Y(1));(X(42),Y(2));(X(42),Y(3));(X(42),Y(4))])
;;

let g12 = Graph([X(13);X(23);X(33);X(43)],
[Y(1);Y(2);Y(3);Y(4)],
[(X(13),Y(1));(X(13),Y(2));(X(13),Y(3));(X(13),Y(4));
 (X(23),Y(4));
 (X(33),Y(1));(X(33),Y(2));(X(33),Y(3));(X(33),Y(4));
 (X(43),Y(1));(X(43),Y(2));(X(43),Y(3));(X(43),Y(4))])
;;



let gs = allDiffs [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12]
;;

iter (printeg) gs;;




let createEdge xs ys = concat (map (fun y -> map (fun x -> (x,y)) xs) ys);;



let ys9 = [Y(1);Y(2);Y(3);Y(4);Y(5);Y(6);Y(7);Y(8);Y(9)]
;;

(*
let ves = [(X(13),Y(3));(X(15),Y(5));(X(17),Y(7));
  (X(22),Y(5));(X(23),Y(8));(X(27),Y(1));(X(28),Y(9));
  (X(32),Y(7));(X(34),Y(1));(X(38),Y(5));
  (X(45),Y(1));(X(46),Y(6));
  (X(52),Y(3));(X(53),Y(7));(X(57),Y(9));(X(58),Y(1));
  (X(62),Y(1));(X(64),Y(7));(X(65),Y(2));(X(68),Y(4));
  (X(72),Y(9));(X(76),Y(5));(X(78),Y(8));
  (X(82),Y(6));(X(83),Y(2));(X(87),Y(5));(X(88),Y(3));
  (X(93),Y(5));(X(95),Y(4));(X(97),Y(6))]
;;
*)

let ves = [(X(11),Y(3));(X(19),Y(1));
  (X(21),Y(5));(X(25),Y(3));(X(28),Y(7));(X(29),Y(4));
  (X(34),Y(1));(X(36),Y(9));
  (X(42),Y(5));
  (X(57),Y(2));(X(58),Y(4));(X(59),Y(8));
  (X(61),Y(7));(X(67),Y(3));
  (X(71),Y(9));(X(74),Y(2));(X(77),Y(8));
  (X(81),Y(4));(X(83),Y(3));(X(86),Y(1));
  (X(92),Y(1));(X(94),Y(9));(X(95),Y(5))]

let g1 = Graph([X(11);X(12);X(13);X(14);X(15);X(16);X(17);X(18);X(19)],ys9,
  createEdge [X(11);X(12);X(13);X(14);X(15);X(16);X(17);X(18);X(19)] ys9)
;;
let g2 = Graph([X(21);X(22);X(23);X(24);X(25);X(26);X(27);X(28);X(29)],ys9,
  createEdge [X(21);X(22);X(23);X(24);X(25);X(26);X(27);X(28);X(29)] ys9)
;;
let g3 = Graph([X(31);X(32);X(33);X(34);X(35);X(36);X(37);X(38);X(39)],ys9,
  createEdge [X(31);X(32);X(33);X(34);X(35);X(36);X(37);X(38);X(39)] ys9)
;;
let g4 = Graph([X(41);X(42);X(43);X(44);X(45);X(46);X(47);X(48);X(49)],ys9,
  createEdge [X(41);X(42);X(43);X(44);X(45);X(46);X(47);X(48);X(49)] ys9)
;;
let g5 = Graph([X(51);X(52);X(53);X(54);X(55);X(56);X(57);X(58);X(59)],ys9,
  createEdge [X(51);X(52);X(53);X(54);X(55);X(56);X(57);X(58);X(59)] ys9)
;;
let g6 = Graph([X(61);X(62);X(63);X(64);X(65);X(66);X(67);X(68);X(69)],ys9,
  createEdge [X(61);X(62);X(63);X(64);X(65);X(66);X(67);X(68);X(69)] ys9)
;;
let g7 = Graph([X(71);X(72);X(73);X(74);X(75);X(76);X(77);X(78);X(79)],ys9,
  createEdge [X(71);X(72);X(73);X(74);X(75);X(76);X(77);X(78);X(79)] ys9)
;;
let g8 = Graph([X(81);X(82);X(83);X(84);X(85);X(86);X(87);X(88);X(89)],ys9,
  createEdge [X(81);X(82);X(83);X(84);X(85);X(86);X(87);X(88);X(89)] ys9)
;;
let g9 = Graph([X(91);X(92);X(93);X(94);X(95);X(96);X(97);X(98);X(99)],ys9,
  createEdge [X(91);X(92);X(93);X(94);X(95);X(96);X(97);X(98);X(99)] ys9)
;;

let g10 = Graph([X(11);X(21);X(31);X(41);X(51);X(61);X(71);X(81);X(91)],ys9,
  createEdge [X(11);X(21);X(31);X(41);X(51);X(61);X(71);X(81);X(91)] ys9)
;;
let g11 = Graph([X(12);X(22);X(32);X(42);X(52);X(62);X(72);X(82);X(92)],ys9,
  createEdge [X(12);X(22);X(32);X(42);X(52);X(62);X(72);X(82);X(92)] ys9)
;;
let g12 = Graph([X(13);X(23);X(33);X(43);X(53);X(63);X(73);X(83);X(93)],ys9,
  createEdge [X(13);X(23);X(33);X(43);X(53);X(63);X(73);X(83);X(93)] ys9)
;;
let g13 = Graph([X(14);X(24);X(34);X(44);X(54);X(64);X(74);X(84);X(94)],ys9,
  createEdge [X(14);X(24);X(34);X(44);X(54);X(64);X(74);X(84);X(94)] ys9)
;;
let g14 = Graph([X(15);X(25);X(35);X(45);X(55);X(65);X(75);X(85);X(95)],ys9,
  createEdge [X(15);X(25);X(35);X(45);X(55);X(65);X(75);X(85);X(95)] ys9)
;;
let g15 = Graph([X(16);X(26);X(36);X(46);X(56);X(66);X(76);X(86);X(96)],ys9,
  createEdge [X(16);X(26);X(36);X(46);X(56);X(66);X(76);X(86);X(96)] ys9)
;;
let g16 = Graph([X(17);X(27);X(37);X(47);X(57);X(67);X(77);X(87);X(97)],ys9,
  createEdge [X(17);X(27);X(37);X(47);X(57);X(67);X(77);X(87);X(97)] ys9)
;;
let g17 = Graph([X(18);X(28);X(38);X(48);X(58);X(68);X(78);X(88);X(98)],ys9,
  createEdge [X(18);X(28);X(38);X(48);X(58);X(68);X(78);X(88);X(98)] ys9)
;;
let g18 = Graph([X(19);X(29);X(39);X(49);X(59);X(69);X(79);X(89);X(99)],ys9,
  createEdge [X(19);X(29);X(39);X(49);X(59);X(69);X(79);X(89);X(99)] ys9)
;;

let g19 = Graph([X(11);X(12);X(13);X(21);X(22);X(23);X(31);X(32);X(33)],ys9,
  createEdge [X(11);X(12);X(13);X(21);X(22);X(23);X(31);X(32);X(33)] ys9)
;;
let g20 = Graph([X(14);X(15);X(16);X(24);X(25);X(26);X(34);X(35);X(36)],ys9,
  createEdge [X(14);X(15);X(16);X(24);X(25);X(26);X(34);X(35);X(36)] ys9)
;;
let g21 = Graph([X(17);X(18);X(19);X(27);X(28);X(29);X(37);X(38);X(39)],ys9,
  createEdge [X(17);X(18);X(19);X(27);X(28);X(29);X(37);X(38);X(39)] ys9)
;;
let g22 = Graph([X(41);X(42);X(43);X(51);X(52);X(53);X(61);X(62);X(63)],ys9,
  createEdge [X(41);X(42);X(43);X(51);X(52);X(53);X(61);X(62);X(63)] ys9)
;;
let g23 = Graph([X(44);X(45);X(46);X(54);X(55);X(56);X(64);X(65);X(66)],ys9,
  createEdge [X(44);X(45);X(46);X(54);X(55);X(56);X(64);X(65);X(66)] ys9)
;;
let g24 = Graph([X(47);X(48);X(49);X(57);X(58);X(59);X(67);X(68);X(69)],ys9,
  createEdge [X(47);X(48);X(49);X(57);X(58);X(59);X(67);X(68);X(69)] ys9)
;;
let g25 = Graph([X(71);X(72);X(73);X(81);X(82);X(83);X(91);X(92);X(93)],ys9,
  createEdge [X(71);X(72);X(73);X(81);X(82);X(83);X(91);X(92);X(93)] ys9)
;;
let g26 = Graph([X(74);X(75);X(76);X(84);X(85);X(86);X(94);X(95);X(96)],ys9,
  createEdge [X(74);X(75);X(76);X(84);X(85);X(86);X(94);X(95);X(96)] ys9)
;;
let g27 = Graph([X(77);X(78);X(79);X(87);X(88);X(89);X(97);X(98);X(99)],ys9,
  createEdge [X(77);X(78);X(79);X(87);X(88);X(89);X(97);X(98);X(99)] ys9)
;;


let gs = map (fun (Graph(xs,ys,es)) -> (Graph(xs,ys,vital ves es))) [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]
;;

iter (printeg) (propaAllDiffs gs)
;;

(*
let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  allDiffs (allDiffs (allDiffs (allDiffs gs)));;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @ 
  (map (diffPropagation (re1,ve1)) [g1])
;; 

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g5;g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g6;g5;g4;g3;g2;g1])
;;
*)
(*
let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g6;g5;g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g7;g6;g5;g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g8;g7;g6;g5;g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g9;g8;g7;g6;g5;g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g10;g9;g8;g7;g6;g5;g4;g3;g2;g1])
;;

let (re1,ve1) = allDiff g1;;

let [g1;g2;g3;g4;g5;g6;g7;g8;g9;g10;g11;g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27] =
  (map (diffPropagation (re1,ve1)) [g12;g13;g14;g15;g16;g17;g18;g19;g20;g21;g22;g23;g24;g25;g26;g27]) @
  (map (diffPropagation (re1,ve1)) [g11;g10;g9;g8;g7;g6;g5;g4;g3;g2;g1])
;;


iter (printeg) (propaAllDiffs gs);;
*)

(*
let [g2;g3;g1]  = (map (diffPropagation (re1,ve1)) [graphe2;graphe3]) @ 
  (map (diffPropagation (re1,ve1)) [graphe1]);;

printeg g1;;
printeg g2;;
printeg g3;;

let (re2',ve2') = allDiff g2;;

let [g3';g2';g1']  = (map (diffPropagation (re2',ve2')) [g3]) @
  (map (diffPropagation (re2',ve2')) [g2;g1]);;

printeg g1';;
printeg g2';;
printeg g3';;

let (Matching(em)) = matching g3'
;;
printe_list em;;

let p33 = useAugmentingPath g3' (X(3)) (Matching(em));;


let (Matching(emMax)) = maximumMatching g3' (Matching(em))

  if (length emMax < length x)
  then raise X_not_matched
  else filterEdges g (Matching(emMax))


let (re3',ve3') = allDiff g3';;


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
