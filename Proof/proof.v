Require Import Coq.Lists.List.
Import ListNotations.
Import Coq.Program.Basics.


Definition space X := list (list X).
Definition goal  X := X -> space X.

Definition none {X}               : space X := [].
Definition unit {X} (s : X)       : space X := [[s]].
Definition step {X} (S : space X) : space X := [] :: S.

Fixpoint take {X} (n : nat) (S : space X) : list (list X) :=
  match n,S with
  | 0,_ => []
  | n, [] => repeat [] n
  | S n', a :: d => a :: take n' d
  end.

Definition sp_eqv {X} (S1 S2 : space X) : Prop := forall n, take n S1 = take n S2.
Hint Unfold sp_eqv.

Module S.
(* BFSser *)

Definition succeed {X} (s : X) : space X := [[s]].
Definition fail    {X} (s : X) : space X := [].

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
  | xs::xss => (zipw (@app X) (map wrap xs) (step (diag xss)))
  end.

Fixpoint transpose {X : Type} (xss : list (list X)) :=
  match xss with
  | [] => []
  | xs::xss => (zipw (@app X) (map wrap xs) (transpose xss))
  end.

Definition shuffle {X} (slsl : list (list (list (list X)))) :=
  map (compose (@concat X) (@concat (list X))) (diag (map transpose slsl)).

Lemma zipwapp_repnil_idem: forall {X} n,
  zipw (@app X) (repeat [] n) (repeat [] n) = (repeat [] n).
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
  take n 
    (map (compose (@concat X) (@concat (list X)))
      (zipw (@app (list (list X))) xss yss))
    =
  take n
    (zipw (@app X)
      (map (compose (@concat X) (@concat (list X))) xss)
      (map (compose (@concat X) (@concat (list X))) yss)).
Proof.
induction n. - auto. -
destruct xss; destruct yss; simpl; auto.
rewrite IHn. unfold compose. simpl.
repeat rewrite concat_app. auto.
Qed.

Lemma mapwrap_zipwapp_distr: forall {X} n (xsss ysss : list (list (list X))),
  take n (map (compose (@concat X) (@concat (list X)))
              (map wrap (zipw (@app (list X)) xsss ysss)))
    =
  take n (map (compose (@concat X) (@concat (list X)))
           (zipw (@app (list (list X)))
              (map wrap xsss)
              (map wrap ysss))).
Proof.
induction n. - auto. -
destruct xsss; destruct ysss; simpl; auto.
rewrite IHn. unfold compose. simpl.
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
  take n (map (compose (concat (A:=X)) (concat (A:=list X))) (map wrap (map wrap xss)))
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
(* BFSimp *)

Fixpoint append_inf {X} s t : space X :=
  match s,t with
  | [],t => t
  | s,[] => s
  | (s0::s),(t0::t) => (s0 ++ t0)::(append_inf s t)
  end.

Fixpoint append_map_inf {X} (g1 : goal X) (S1 : space X) : space X :=
  match S1 with
  | [] => []
  | s1::S1' => append_inf
                 (fold_right append_inf none (map g1 s1))
                 (step (append_map_inf g1 S1'))
  end.

Lemma append_inf_repnil_idem: forall {X} n,
  @append_inf X (repeat [] n) (repeat [] n) = (repeat [] n).
Proof.
induction n.
- auto.
- simpl. rewrite IHn. auto.
Qed.

Lemma append_inf_repnil_l: forall {X} n (S : space X),
  append_inf (repeat [] n) (take n S) = (take n S).
Proof.
induction n.
- auto.
- destruct S; simpl.
  + rewrite append_inf_repnil_idem. auto.
  + rewrite IHn. auto.
Qed.

Lemma append_inf_repnil_r: forall {X} n (S : space X),
  append_inf (take n S) (repeat [] n) = (take n S).
Proof.
induction n.
- auto.
- destruct S; simpl.
  + rewrite append_inf_repnil_idem. auto.
  + rewrite IHn. rewrite app_nil_r. auto.
Qed.
End O.

Lemma zipwapp_is_append_inf: forall {X} n xss yss,
  take n (S.zipw (@app X) xss yss)
    =
  take n (O.append_inf xss yss).
Proof.
induction n. - auto.
- destruct xss; destruct yss; simpl; auto.
  rewrite IHn. auto.
Qed.

Lemma mapcc_zipwapp_distr: forall {X} n (xss yss : list (list (list (list X)))),
  take n (map (compose (@concat X) (@concat (list X))) (S.zipw (@app (list (list X))) xss yss))
    =
  take n (S.zipw (@app X)
            (map (compose (@concat X) (@concat (list X))) xss)
            (map (compose (@concat X) (@concat (list X))) yss)).
Proof.
induction n.
- auto.
- intros. destruct xss; destruct yss; simpl; auto.
  rewrite <- IHn. unfold compose. 
  repeat rewrite concat_app. auto.
Qed.

Lemma take_append_inf_distr: forall {X} n (S1 S2 : list (list X)),
  take n (O.append_inf S1 S2) = O.append_inf (take n S1) (take n S2).
Proof.
induction n.
- auto.
- destruct S1; destruct S2; simpl.
  + rewrite O.append_inf_repnil_idem. auto.
  + rewrite O.append_inf_repnil_l. auto.
  + rewrite O.append_inf_repnil_r. rewrite app_nil_r. auto.
  + rewrite IHn. auto.
Qed.

Theorem disj_relate: forall {X} (S1 S2 S1' S2': space X),
  sp_eqv S1 S1' -> sp_eqv S2 S2' ->
    sp_eqv (O.append_inf S1 S2) (S.zipw (@app X) S1' S2').
Proof.
autounfold. intros. rewrite zipwapp_is_append_inf.
repeat rewrite take_append_inf_distr. rewrite H. rewrite H0. auto.
Qed.

Theorem conj_relate: forall {X} (g1 g1': goal X) (S1 S1': space X),
  sp_eqv S1 S1' -> (forall x, sp_eqv (g1 x) (g1' x)) ->
    sp_eqv (O.append_map_inf g1 S1) (S.shuffle (S.mmap g1 S1)). 
Proof.
autounfold. intros. unfold S.shuffle. unfold S.mmap.
generalize S1. induction n. auto.
destruct S0. { auto. }
Opaque take. simpl. remember (map g1 l) as Ss. clear HeqSs.
rewrite mapcc_zipwapp_distr.
rewrite zipwapp_is_append_inf.
rewrite <- S.take_idem. rewrite take_append_inf_distr. simpl.
symmetry.
rewrite <- S.take_idem. rewrite take_append_inf_distr. simpl.
assert ((take (S n)
        (map (compose (concat (A:=X)) (concat (A:=list X)))
           (map S.wrap (S.transpose Ss)))) = 
        (take (S n) (fold_right O.append_inf none Ss))).
{ clear. generalize (S n). intros. induction Ss. auto.
  simpl.
  rewrite take_append_inf_distr.
  rewrite <- IHSs.
  rewrite S.mapwrap_zipwapp_distr.
  rewrite S.mapcc_zipwapp_distr.
  rewrite S.take_zipwapp_distr.
  rewrite S.mapcc_wrapwrap. 
  rewrite <- S.take_zipwapp_distr.
  rewrite <- take_append_inf_distr.
  apply zipwapp_is_append_inf.
}
assert ((take (S n)
        (compose (concat (A:=X)) (concat (A:=list X)) []
         :: map (compose (concat (A:=X)) (concat (A:=list X)))
              (S.diag (map S.transpose (map (map g1) S0))))) =
        (take (S n) (step (O.append_map_inf g1 S0)))).
{ unfold step. unfold compose. simpl.
  Transparent take. simpl. rewrite IHn.
  unfold compose. auto. }
rewrite H1. rewrite H2. auto.
Qed.