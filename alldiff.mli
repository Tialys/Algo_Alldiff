

val x_g : graphe -> node list
(* Retourne l'ensemble de sommets X du graphe biparti (X,Y,E) *)

val y_g : graphe -> node list
(* Retourne l'ensemble de sommets Y du graphe biparti (X,Y,E) *)

val e_g : graphe -> edge list
(* Retourne l'ensemble de arretes E du graphe biparti (X,Y,E) *)

val voisin : node -> graphe -> node list
(* voisin x g Retourne la liste des sommets voisin de x dans g *)

val couplage : graphe -> edge list
(* couplage g retourne une liste d'arretes de g qui constitue un couplage de g *)

val chaine_alternante : node -> graphe -> edge list -> edge list
(* chaine_alternante x g edge_sat  renvoie une chaine alternante partant de x *)




