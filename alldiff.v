Require Import List.
Import ListNotations.


Variable graph : Set.
Variable nodeX : Type.
Variable nodeY : Type.

Definition edge := (nodeX * nodeY) %type.
Definition matching := list edge %type.

Variable nodeX_eq_dec : forall x y : nodeX,
{x = y} + {~(x = y)}.
Variable nodeY_eq_dec : forall x y : nodeY,
{x = y} + {~(x = y)}.

Definition is_matching (m : matching) : Prop := 
forall e1 e2 : edge, In e1 m /\ In e2 m -> fst e1 = fst e2 -> snd e1 = snd e2.

Definition is_covering_X (m : matching) (xs : list nodeX) : Prop :=
forall x : nodeX, In x xs -> exists e : edge, In e m /\ x = fst e.

Definition is_covering_Y (m : matching) (ys : list nodeY) : Prop :=
forall y : nodeY, In y ys -> exists e : edge, In e m /\ y = snd e.

Definition is_covering (m : matching) (xs : list nodeX) (ys : list nodeY) : Prop :=
forall (x : nodeX) (y : nodeY), is_covering_X m (x::nil) /\ is_covering_Y m (y::nil).

Definition is_augmenting_path (p : list edge) (m : matching) : Prop :=
forall (e1 e2 : edge), In e1 p /\ In e2 p -> (fst e1 = fst e2) /\ (snd e1 <> snd e2) -> 
((In e1 m) /\ ~(In e2 m)) \/ (~(In e1 m) /\ (In e2 m)).


Fixpoint first (l : list edge) : edge :=
match l with
  | [] => None
  | e :: l => e
end.

Fixpoint last (l : list edge) : edge :=
match l with
  | [] => nil
  | [e] => e
  | e::l => last l
end.

Definition is_augmenting_path (p : list edge) (m : matching) : Prop :=
~(is_covering m (fst (hd_error p)) (snd (tl nil p))).

Definition example1 (e : edge) (n1 : nodeX) (n2 : nodeY) (n3 : nodeY) : matching := (n1,n2)::(n1,n3)::nil.
Check example1.

Variable n1 : nodeX.
Variable n2 : nodeY.
Variable n3 : nodeY.
Variable e : edge.
Eval compute in is_matching (example1 e n1 n2 n3).

Definition is_augmenting_path (p : list edge) (c : matching) : Prop :=
match p with
| [e] => ~(In e c)
| e1::e2::es => In e1 c
end.


forall (e1 e2 : edge) (c : matching), (fst e1 = fst e2) /\ (snd e1 <> snd e2) -> 
((In e1 c) /\ ~(In e2 c)) \/ (~(In e1 c) /\ (In e2 c)).

Definition covering_matching (


Lemma test : forall e1 e2 : edge,
(fst e1 = fst e2).

Check matching.



