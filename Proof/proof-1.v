Require Import Coq.Lists.List.
Import ListNotations.
Import Coq.Program.Basics.
Require Import Coq.Logic.FunctionalExtensionality.

Variable state : Type.



Module S.

Definition space := list (list state).
Definition goal := state -> space.

Definition succeed (s : state) : space := [[s]].
Definition fail    (s : state) : space := [].
Hint Unfold succeed.
Hint Unfold fail.

Definition mmap {A B} : (A -> B) -> (list (list A)) -> (list (list B)) :=
  compose (@map (list A) (list B)) (@map A B).
Definition wrap {X : Type} (x : X) := [x].

Fixpoint zipw {X: Type} (f : X -> X -> X) (xs ys : list X) : list X :=
  match xs,ys with
  | [],ys => ys
  | xs,[] => xs
  | (x::xs),(y::ys) => (f x y)::(zipw f xs ys)
  end.

Fixpoint diag {X : Type} (xss : list (list X)) :=
  match xss with
  | [] => []
  | []::xss => []::(diag xss)
  | (x::xs)::xss => [x]::(zipw (@app X) (map wrap xs) (diag xss))
  end.

Fixpoint transpose {X : Type} (xss : list (list X)) :=
  match xss with
  | [] => []
  | xs::xss => (zipw (@app X) (map wrap xs) (transpose xss))
  end.

Definition shuffle (slsl : list (list (list (list state)))) :=
  map (compose (@concat state) (@concat (list state))) (diag (map transpose slsl)).

Definition disj2 (g1 g2 : goal) (s : state) := 
  zipw (@app state) (g1 s) (g2 s).
Definition conj2 (g1 g2 : goal) (s : state) := 
  shuffle (mmap g2 (g1 s)).
Hint Unfold disj2.
Hint Unfold conj2.

Fixpoint take {X} (n : nat) (S : list (list X)) : list (list X) :=
  match n,S with
  | 0,_ => []
  | n, [] => repeat [] n
  | S n', a :: d => a :: take n' d
  end.

Lemma zipwapp_repnil_idem: forall {X} n,
  S.zipw (@app X) (repeat [] n) (repeat [] n) = (repeat [] n).
Proof.
induction n.
- auto.
- simpl. rewrite IHn. auto.
Qed.

Lemma zipwapp_repnil_l: forall {X} n (S : list (list X)),
  zipw (@app X) (repeat [] n) (take n S) = (take n S).
Proof.
induction n.
- auto.
- destruct S; simpl.
  + rewrite zipwapp_repnil_idem. auto.
  + rewrite IHn. auto.
Qed.

Lemma zipwapp_repnil_r: forall {X} n (S : list (list X)),
  zipw (@app X) (take n S) (repeat [] n) = (take n S).
Proof.
induction n.
- auto.
- destruct S; simpl.
  + rewrite zipwapp_repnil_idem. auto.
  + rewrite IHn. rewrite app_nil_r. auto.
Qed.

Lemma take_zipwapp_distr: forall {X} n (S T : list (list X)),
  take n (zipw (@app X) S T) = zipw (@app X) (take n S) (take n T).
Proof.
induction n.
- auto.
- destruct S; destruct T; simpl.
  + rewrite zipwapp_repnil_idem. auto.
  + rewrite zipwapp_repnil_l. auto.
  + rewrite zipwapp_repnil_r. rewrite app_nil_r. auto.
  + rewrite IHn. auto.
Qed.

Lemma take_nil: forall {X} n, @take X n [] = repeat [] n.
Proof. intros. destruct n; auto. Qed.

Lemma mapcc_nil: forall {X} n,
  map (compose (concat (A:=X)) (concat (A:=list X))) (repeat [] n) = repeat [] n.
Proof.
induction n.
- auto.
- simpl. rewrite IHn. auto.
Qed.

Lemma take_mapcc_comm: forall {X} n xss,
  take n (map (compose (@concat X) (@concat (list X))) xss) = 
  map (compose (@concat X) (@concat (list X))) (take n xss).
Proof.
intros. generalize dependent xss.
induction n.
- auto.
- intros. destruct xss.
  + unfold compose. simpl. 
    { clear IHn. induction n.
      + auto.
      + simpl. rewrite IHn; auto. }
  + simpl. rewrite IHn. auto.
Qed.

Lemma mapcc_zipwapp_distr: forall {X} n (xss yss : list (list (list (list X)))),
  map (compose (@concat X) (@concat (list X)))
    (zipw (@app (list (list X)))
      (take n xss)
      (take n yss)) =
  zipw (@app X)
    (map (compose (@concat X) (@concat (list X))) (take n xss))
    (map (compose (@concat X) (@concat (list X))) (take n yss)).
Proof.
induction n.
- auto.
- destruct xss; destruct yss; simpl.
  + rewrite zipwapp_repnil_idem.
    specialize (IHn [] []). simpl in IHn. 
    repeat rewrite take_nil in IHn. 
    rewrite <- IHn.
    rewrite zipwapp_repnil_idem. auto.
  + repeat rewrite zipwapp_repnil_l.
    rewrite mapcc_nil.
    rewrite <- take_mapcc_comm.
    rewrite zipwapp_repnil_l. auto.
  + specialize (IHn xss []).
    simpl in IHn. repeat rewrite take_nil in IHn.
    rewrite IHn.
    repeat unfold compose. simpl. 
    repeat rewrite app_nil_r. auto.
  + specialize (IHn xss yss).
    rewrite IHn. 
    repeat unfold compose. simpl. 
    repeat rewrite concat_app.
    auto.
Qed.

Lemma mapwrap_zipwapp_distr: forall {X} n (xsss ysss : list (list (list X))),
  map (compose (@concat X) (@concat (list X)))
   (map wrap (zipw (@app (list X)) (take n xsss) (take n ysss)))
    =
  map (compose (@concat X) (@concat (list X)))
   (zipw (@app (list (list X)))
     (map wrap (take n xsss))
     (map wrap (take n ysss))).
Proof.
unfold wrap. unfold compose.
induction n.
- auto.
- destruct xsss; destruct ysss; simpl.
  + rewrite zipwapp_repnil_idem.
    specialize (IHn [] []). simpl in IHn. 
    repeat rewrite take_nil in IHn. 
    rewrite <- IHn.
    rewrite zipwapp_repnil_idem. auto.
  + repeat rewrite zipwapp_repnil_l. simpl.
    specialize (IHn [] ysss).
    repeat rewrite take_nil in IHn.
    rewrite <- IHn. rewrite zipwapp_repnil_l. auto.
  + repeat rewrite zipwapp_repnil_l. simpl.
    specialize (IHn xsss []).
    repeat rewrite take_nil in IHn.
    rewrite <- IHn. rewrite zipwapp_repnil_r. 
    repeat rewrite app_nil_r. auto.
  + specialize (IHn xsss ysss).
    rewrite IHn. 
    repeat unfold compose. simpl. 
    repeat rewrite concat_app.
    rewrite app_assoc. auto.
Qed.

Lemma take_mapwrap_comm: forall {X} n (xss : list (list (list X))),
  map (compose (@concat X) (@concat (list X))) (take n (map wrap xss))
    =
  map (compose (@concat X) (@concat (list X))) (map wrap (take n xss)).
Proof.
induction n. auto.
intros. destruct xss.
- simpl. specialize (IHn []). simpl in IHn. repeat rewrite take_nil in IHn.
  rewrite <- IHn. auto.
- simpl. rewrite IHn. auto.
Qed.

Lemma take_idem: forall {X} n (xss : list (list X)),
  take n (take n xss) = take n xss.
Proof.
induction n. auto. 
intros. destruct xss; simpl.
- specialize (IHn []). repeat rewrite take_nil in IHn. rewrite IHn. auto.
- rewrite IHn. auto.
Qed.

Lemma mapcc_wrapwrap: forall {X} n (xss : list (list X)), 
  take n (map (compose (concat (A:=X)) (concat (A:=list X))) (map S.wrap (map S.wrap xss)))
    =
  take n xss.
Proof.
induction n. auto. 
intros. destruct xss; simpl.
- specialize (IHn []). repeat rewrite take_nil in IHn. auto.
- rewrite IHn. unfold compose. simpl. rewrite app_nil_r. auto.
Qed.

End S.



Module O.

Inductive space : Type :=
  | sp : list state -> option space -> space.
Definition goal := state -> space.

Definition none   := sp  [] None.
Definition unit s := sp [s] None.
Definition step d := sp  [] d.

Definition succeed (s : state) := unit s.
Definition fail    (s : state) := none.
Hint Unfold succeed.
Hint Unfold fail.

Fixpoint Sapp S1 S2 :=
  match S1, S2 with
  | sp a1 d1, sp a2 d2 =>
    sp (a1 ++ a2)
       match d1,d2 with
       | None, d => d
       | d, None => d
       | Some s1, Some s2 => Some (Sapp s1 s2)
       end
  end.

Fixpoint Sappmap g S :=
  match S with
  | sp a d =>
    fold_right
      (fun s S' => Sapp (g s) S')
      (step (match d with
             | None => None
             | Some T => Some (Sappmap g T)
             end))
      a
  end.

(* more proof-friendly *)
Fixpoint Sappmap' g S := 
  match S with
  | sp a d =>
    Sapp
      (fold_right Sapp none (map g a))
      (step (match d with
             | None => None
             | Some T => Some (Sappmap g T)
             end))
  end.

Definition disj2 (g1 g2 : goal) (s : state) := 
  Sapp (g1 s) (g2 s).
Definition conj2 (g1 g2 : goal) (s : state) := 
  Sappmap g2 (g1 s).
Hint Unfold disj2.
Hint Unfold conj2.

Fixpoint take n S :=
  match n,S with
  | 0,_ => []
  | S n', sp a (None) => a :: repeat [] n'
  | S n', sp a (Some T) => a :: take n' T
  end.

Lemma Sappmap_eqv: forall n g1 S1, 
take n (Sappmap g1 S1) = take n (Sappmap' g1 S1).
Proof.
intros. generalize S1.
induction n. { auto. }
intros. destruct S0. destruct o.
Admitted.

Definition zipwapp_repnil_idem {X} := @S.zipwapp_repnil_idem X.

Lemma zipwapp_repnil_l: forall n S1,
  S.zipw (@app state) (repeat [] n) (take n S1) = (take n S1).
Proof.
induction n.
- auto.
- destruct S1 as [a1 d1]; destruct d1; simpl.
  + rewrite IHn. auto.
  + rewrite zipwapp_repnil_idem. auto.
Qed.

Lemma zipwapp_repnil_r: forall n S1,
  S.zipw (@app state) (take n S1) (repeat [] n) = (take n S1).
Proof.
induction n.
- auto.
- destruct S1 as [a1 d1]; destruct d1; simpl.
  + rewrite IHn. rewrite app_nil_r. auto.
  + rewrite zipwapp_repnil_idem. rewrite app_nil_r. auto.
Qed.

Lemma take_app_distr: forall n (S1 S2 : space),
  take n (Sapp S1 S2) = S.zipw (@app state) (take n S1) (take n S2).
Proof.
induction n.
- auto.
- destruct S1 as [a1 d1]; destruct d1 as [| S1'];
  destruct S2 as [a2 d2]; destruct d2 as [| S2'];
  simpl.
  + rewrite IHn. auto.
  + rewrite zipwapp_repnil_r. auto.
  + rewrite zipwapp_repnil_l. auto.
  + rewrite zipwapp_repnil_idem. auto.
Qed.

End O.


Definition space_relate S S' : Prop :=
  forall n, O.take n S = S.take n S'.
Hint Unfold space_relate.
Definition goal_relate g g' : Prop :=
  forall (s : state), space_relate (g s) (g' s).
Hint Unfold goal_relate.

Theorem succeed_relate : goal_relate O.succeed S.succeed.
Proof.
autounfold. unfold O.succeed. unfold O.unit. unfold S.succeed.
unfold space_relate.
destruct n; simpl. auto. destruct n; auto.
Qed.

Theorem fail_relate: goal_relate O.fail S.fail.
Proof.
repeat autounfold. unfold O.fail. unfold O.none. unfold S.fail.
intros. destruct n; auto.
Qed.

Lemma Sapp_zipwapp_relate: forall S1 S2 S1' S2',
  space_relate S1 S1' -> space_relate S2 S2' ->
    space_relate (O.Sapp S1 S2) (S.zipw (@app state) S1' S2').
Proof.
autounfold. intros.
rewrite S.take_zipwapp_distr.
rewrite O.take_app_distr. 
rewrite H. rewrite H0. auto.
Qed.

Theorem disj_relate : forall g1 g2 g1' g2',
  goal_relate g1 g1' -> goal_relate g2 g2' -> 
  goal_relate (O.disj2 g1 g2) (S.disj2 g1' g2').
Proof.
intros g1 g2 g1' g2' H1 H2 s n. 
autounfold.
specialize (H1 s).
specialize (H2 s).
remember (g1 s) as S1.
remember (g2 s) as S2.
remember (g1' s) as S1'.
remember (g2' s) as S2'.
clear dependent g1.
clear dependent g2.
clear dependent g1'.
clear dependent g2'.
autounfold. autounfold in H1. autounfold in H2.
(* core part starts here *)
apply Sapp_zipwapp_relate; auto.
Qed.

(* more proof-friendly *)
Fixpoint diag {X : Type} (xss : list (list X)) :=
  match xss with
  | [] => []
  | xs::xss => S.zipw (@app X) (map S.wrap xs) ([]::(diag xss))
  end.

Lemma diag_eqv: forall {X} n (xss : list (list X)),
  S.take n (S.diag xss) = S.take n (diag xss).
Proof.
induction n.
- auto.
- destruct xss. 
  + auto.
  + destruct l.
    * simpl. rewrite IHn. auto.
    * simpl. 
      repeat rewrite S.take_zipwapp_distr.
      rewrite IHn. auto.
Qed.

Theorem conj_relate : forall g1 g2 g1' g2',
  goal_relate g1 g1' -> goal_relate g2 g2' -> 
  goal_relate (O.conj2 g1 g2) (S.conj2 g1' g2').
Proof.
autounfold.
intros g1 g2 g1' g2' H1 H2 s n. 
specialize (H1 s).
remember (g1 s) as S1.
remember (g1' s) as S1'.
clear dependent g1.
clear dependent g1'.
unfold S.shuffle.
(* core part starts here *)
induction n. { auto. }
destruct S1 as [l oS]; destruct S1' as [| l'].
- admit.
- specialize (H1 (S n)). inversion H1. subst. (* the first bags are the same *)
  rewrite diag_eqv.
  Opaque diag. Opaque O.Sappmap. Opaque O.take. Opaque S.take. simpl.
  rewrite O.Sappmap_eqv. simpl.
  Transparent diag. simpl.
  rewrite S.take_zipwapp_distr.
  rewrite S.mapcc_zipwapp_distr.
  repeat rewrite <- S.take_mapcc_comm.
  rewrite <- S.take_zipwapp_distr.
  apply Sapp_zipwapp_relate.
  { unfold space_relate. intros. generalize dependent H2. generalize dependent s. clear.
    intros. autounfold in H2.
    induction l'.
    - simpl. pose fail_relate. repeat autounfold in g.
      autounfold.
      apply g. auto.
    - simpl. unfold space_relate. intros.
      rewrite O.take_app_distr.
      rewrite IHl'. rewrite H2. clear. 
      (* the rest is just about moving things around *)
      remember (g2' a).  clear Heql.
      repeat rewrite S.take_mapcc_comm.
      symmetry.
      rewrite <- S.take_idem.
      rewrite <- S.take_mapcc_comm.
      rewrite S.take_mapwrap_comm.
      rewrite S.take_mapcc_comm.
      rewrite S.take_zipwapp_distr.
      rewrite <- S.take_mapcc_comm.
      rewrite S.mapwrap_zipwapp_distr.
      rewrite S.take_mapcc_comm.
      rewrite S.take_zipwapp_distr.
      rewrite S.mapcc_zipwapp_distr.
      rewrite <- S.take_mapcc_comm.
      rewrite <- S.take_mapcc_comm.
      rewrite <- S.take_mapwrap_comm.
      rewrite <- S.take_mapcc_comm.
      rewrite S.take_idem.
      rewrite <- S.take_mapwrap_comm.
      rewrite <- S.take_mapcc_comm.
      rewrite S.take_idem.
      rewrite S.mapcc_wrapwrap. auto.
  }
  { unfold space_relate. intros.
    unfold O.step. destruct n0; auto.
    Transparent O.take. simpl. }
- admit.
- admit.
Admitted.










