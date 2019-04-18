Require Import Coq.Lists.List.
Import ListNotations.
Import Coq.Program.Basics.
Require Import Coq.Logic.FunctionalExtensionality.

Definition state := nat.
Definition stream (X : Type) := list (list X).
Definition sstream := stream state.
Definition goal := state -> stream state.

Fixpoint take {X} (n : nat) (s : stream X) : stream X :=
  match n,s with
  | 0,_ => []
  | S n, [] => [] :: (take n [])
  | S n, x :: xs => x :: (take n xs)
  end.

Definition delimit (n : nat) (g : goal) := fun s => take n (g s).
Definition run (n : nat) (g : goal) (s : state) := take n (g s).

Lemma take_length: forall {X} n (s : stream X), length (take n s) = n.
Proof.
intros X n. induction n.
- reflexivity.
- simpl. destruct s; (simpl; rewrite IHn; auto).
Qed.

Lemma take_idem : forall {X} n (s : stream X), take n (take n s) = take n s.
Proof.
induction n.
- simpl. auto.
- destruct s;
  (simpl; rewrite IHn; auto).
Qed.

(* Our BFS *)

Fixpoint append_inf s t : sstream :=
  match s,t with
  | [],t => t
  | s,[] => s
  | (s0::s),(t0::t) => (s0 ++ t0)::(append_inf s t)
  end.

Fixpoint append_map_inf (g : goal) (s : sstream) : sstream :=
  match s with
  | [] => []
  | s0::s => fold_right append_inf
               ([]::(append_map_inf g s))
               (map g s0)
  end.

Definition mk_disj2 g1 g2 := fun (s : state) => append_inf (g1 s) (g2 s).
Definition mk_conj2 g1 g2 := fun (s : state) => append_map_inf g2 (g1 s).

(* Silvija's BFS *)

Definition mmap {X Y} (f : X -> Y) (s : stream X) := map (fun xs => map f xs) s.
Definition wrap {X : Type} (x : X) := [x].

Fixpoint zipw {X : Type} (f : X -> X -> X) (xs ys : list X) :=
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

Definition shuffle (slsl : stream (sstream)) :=
  map (fun lll => concat (concat lll)) (diag (map transpose slsl)).

Definition seres_disj2 (g1 g2 : goal) (s : state) := 
  zipw (@app state) (g1 s) (g2 s).
Definition seres_conj2 (g1 g2 : goal) (s : state) := 
  shuffle (mmap g2 (g1 s)).


Definition obs_eqv g1 g2 := forall n, run n g1 = run n g2.

Lemma append_inf_is_zipw_app : append_inf = zipw (@app state).
Proof.
extensionality s1.
induction s1.
- simpl. auto.
- extensionality s2.
  simpl. rewrite IHs1. auto.
Qed.

Theorem disj2_eqv : forall g1 g2, obs_eqv (mk_disj2 g1 g2) (seres_disj2 g1 g2).
Proof.
intros. unfold obs_eqv. unfold run. unfold mk_disj2. unfold seres_disj2.
rewrite <- append_inf_is_zipw_app. auto.
Qed.


Lemma take_one: forall {X} n (x : list X) (xs : stream X),
  take (S n) (x :: xs) = x :: (take n xs).
Proof.
simpl. auto.
Qed.

Lemma zipw_nil_r: forall {X} (f : X -> X -> X) (xs : list X),
  zipw f xs [] = xs.
Proof.
destruct xs; auto.
Qed.

Lemma concat_wrap: forall {X} (xss : list (list X)),
  map (@concat X) (map wrap xss) = xss.
Proof.
induction xss.
- auto.
- simpl. rewrite IHxss. rewrite app_nil_r. auto.
Qed.

Lemma map_concat_step: forall {X} (xsss : list (list (list X))),
  map (@concat X) ([]::xsss) = []::(map (@concat X) xsss).
Proof. simpl. auto. Qed.

Lemma map_cconcat: forall {X} (xsss : list (list (list (list X)))),
  map (fun lll : list (list (list X)) => concat (concat lll)) xsss =
  map (@concat X) (map (@concat (list X)) xsss).
Proof.
intros. induction xsss.
- simpl. auto.
- simpl. rewrite IHxsss. auto.
Qed.

Lemma mapc_zipw_dist: forall {X} (xs ys : list (list (list X))),
  map (@concat X) (zipw (@app (list X)) xs ys) = 
  zipw (@app X) (map (@concat X) xs) (map (@concat X) ys).
Proof.
induction xs.
- simpl. auto.
- simpl. destruct ys.
  + simpl. auto.
  + simpl. rewrite IHxs. 
    rewrite concat_app. auto.
Qed.
Lemma lem5: forall l5 l4,
  map (concat (A:=state))
     (map (concat (A:=list state))
        (zipw (app (A:=list (list state))) (map wrap l5) (map wrap l4))) =
  map (concat (A:=state))
     (map (concat (A:=list state))
        (map wrap (zipw (app (A:=list state)) l5 l4))).
Proof.
induction l5.
- simpl. auto.
- intros. destruct l4.
  + simpl. auto.
  + simpl. rewrite IHl5. rewrite app_assoc. auto.
Qed.

Lemma zipw_app_asso: forall {X} (xs ys zs : list (list X)),
  zipw (@app X) (zipw (@app X) xs ys) zs = zipw (@app X) xs (zipw (@app X) ys zs).
Proof.
induction xs.
- simpl. auto.
- destruct ys; destruct zs; auto.
  simpl. rewrite IHxs. rewrite app_assoc. auto.
Qed.

Lemma lem4: forall l5 l4 l7,
  map (concat (A:=state))
     (map (concat (A:=list state))
        (zipw (app (A:=list (list state)))
           (zipw (app (A:=list (list state))) (map wrap l5) (map wrap l4))
           l7)) =
  map (concat (A:=state))
     (map (concat (A:=list state))
        (zipw (app (A:=list (list state)))
           (map wrap (zipw (@app (list state)) l5 l4))
           l7)).
Proof.
induction l5.
- simpl. auto.
- intros. destruct l4; destruct l7; auto.
  + simpl.
    rewrite <- (zipw_nil_r (app (A:=list (list state))) (map wrap (zipw (app (A:=list state)) l5 l4))).
    rewrite <- IHl5. 
    rewrite zipw_app_asso.
    rewrite zipw_nil_r. 
    rewrite app_assoc. auto.
  + simpl. rewrite <- IHl5. 
    rewrite app_assoc. auto.
Qed.

Lemma lem3: forall s m l0,
  append_inf s (fold_right append_inf ([] :: shuffle m) l0) = shuffle ((s :: l0) :: m).
Proof.
intros. unfold shuffle. repeat rewrite map_cconcat.
Opaque diag. simpl.
generalize dependent (map transpose m). intros.
generalize dependent s.
induction l0.
- intros. simpl. rewrite zipw_nil_r. rewrite append_inf_is_zipw_app.
  rewrite <- (concat_wrap s).
  rewrite <- map_concat_step.
  rewrite <- mapc_zipw_dist.
  rewrite <- map_concat_step.
  rewrite <- (concat_wrap (map wrap s)).
  rewrite <- mapc_zipw_dist.
  repeat rewrite concat_wrap. destruct (map wrap s).
  + simpl. Transparent diag. simpl. auto.
  + simpl. auto.
- Opaque diag. simpl. rewrite IHl0. intros. clear.
  rewrite append_inf_is_zipw_app.
  rewrite <- (concat_wrap s).
  rewrite <- mapc_zipw_dist.
  rewrite <- (concat_wrap (map wrap s)).
  rewrite <- mapc_zipw_dist.
  repeat rewrite concat_wrap.
  generalize dependent (map wrap s). intros.
  generalize dependent (map wrap a). intros.
  generalize dependent (transpose l0). intros.
  generalize dependent (zipw (app (A:=list state)) l2 l3). intros.
  {
  (* I want to do induction on l, l1 and l4. *)
  generalize dependent l1. generalize dependent l.
  Transparent diag. induction l4.
  - intros. destruct l1; (simpl; auto).
  - intros. destruct l1.
    + simpl. auto.
    + simpl. destruct l.
      * simpl. repeat rewrite zipw_nil_r. rewrite lem5.
        rewrite app_assoc. auto.
      * rewrite <- zipw_app_asso. rewrite app_assoc.
        rewrite lem4. auto.
  }
Qed.

Lemma append_inf_take_nil_l: forall s n, append_inf (take n []) (take n s) = take n s.
Proof.
induction s.
- induction n.
  + auto.
  + simpl. rewrite IHn. auto.
- destruct n.
  + auto.
  + simpl. rewrite IHs. auto.
Qed.

Lemma append_inf_take_nil_r: forall s n, append_inf (take n s) (take n []) = take n s.
Proof.
induction s.
- induction n.
  + auto.
  + simpl. rewrite IHn. auto.
- destruct n.
  + auto.
  + simpl. rewrite app_nil_r. rewrite IHs. auto.
Qed.

Lemma take_app_comm : forall s1 s2 n,
  take n (append_inf s1 s2) = append_inf (take n s1) (take n s2).
Proof.
induction s1.
* intros. 
  rewrite append_inf_take_nil_l. auto.
* destruct s2.
  - intros. rewrite append_inf_take_nil_r. 
    simpl. auto.
  - destruct n. 
    + simpl. auto.
    + simpl. rewrite IHs1. auto.
Qed.

Lemma take_fold_right_app_comm : forall n s ls,
  take n (fold_right append_inf s ls) = fold_right append_inf (take n s) (map (take n) ls).
Proof.
induction ls.
- simpl. auto.
- simpl. rewrite take_app_comm. rewrite IHls. auto.
Qed.

Lemma lem2: forall n g1 s1,
  take n (append_map_inf g1 s1) = take n (shuffle (mmap g1 s1)).
Proof.
induction n.
- intros. simpl. Transparent take. simpl. auto.
- intros. destruct s1.
  + unfold append_map_inf. simpl. auto.
  + Opaque take. simpl. destruct (map g1 l).
    * simpl. rewrite take_one. rewrite IHn.
      {
      clear. unfold shuffle. repeat rewrite map_cconcat.
      simpl. rewrite take_one. auto.
      }
    * simpl. rewrite take_app_comm. rewrite take_fold_right_app_comm.
      rewrite take_one. rewrite IHn.
      rewrite <- take_one. rewrite <- take_fold_right_app_comm.
      rewrite <- take_app_comm.
      rewrite lem3. auto.
Qed.

Theorem conj2_eqv : forall g1 g2, obs_eqv (mk_conj2 g1 g2) (seres_conj2 g1 g2).
Proof.
intros. unfold obs_eqv. unfold run. unfold mk_conj2. unfold seres_conj2.
intros. extensionality s.
generalize dependent n.
intros. apply lem2.
Qed.

Lemma limit_append_inf: forall n s1 s2,
  take n (append_inf s1 s2) = take n (append_inf (take n s1) (take n s2)).
Proof.
induction n.
- simpl. auto.
- intros. destruct s1; destruct s2; simpl;
  try rewrite append_inf_take_nil_r;
  try rewrite append_inf_take_nil_l;
  try rewrite take_idem.
  + auto.
  + auto.
  + auto. 
  + Transparent take. simpl. rewrite IHn. auto.
Qed.

Theorem limit_mk_disj2 : forall n g1 g2,
  run n (mk_disj2 g1 g2) = run n (mk_disj2 (delimit n g1) (delimit n g2)).
(* we can throw away later information in disj2 *)
Proof.
unfold run. unfold mk_disj2. unfold delimit.
intros. extensionality s.
apply limit_append_inf.
Qed.

Lemma append_map_inf_nil : forall n g1,
  append_map_inf g1 (take n []) = take n [].
Proof.
intros. induction n.
- simpl. auto.
- simpl. rewrite IHn. auto.
Qed.

Lemma take_refine: forall {X} n (s1 : stream X),
  take n (take (S n) s1) = take n s1.
Proof.
induction n.
- simpl. auto.
- intros. simpl. destruct s1.
  + specialize (IHn []). simpl in IHn. rewrite IHn. auto.
  + specialize (IHn s1). simpl in IHn. rewrite IHn. auto.
Qed.

Lemma delimit_refine: forall n g1,
  delimit n (delimit (S n) g1) = delimit n g1.
Proof.
unfold delimit.
intros.
extensionality s.
apply take_refine.
Qed.


Lemma map_map_compose: forall {X Y Z: Type} (f : Y -> Z) (g : X -> Y) (xs : list X),
  map f (map g xs) = map (fun x => (f (g x))) xs.
Proof.
induction xs.
- simpl. auto.
- simpl. rewrite IHxs. auto.
Qed.

Lemma limit_append_map_inf : forall n g1 s1,
  take n (append_map_inf g1 s1) = take n (append_map_inf (delimit n g1) (take n s1)).
Proof.
induction n.
- simpl. auto.
- destruct s1.
  + simpl. rewrite append_map_inf_nil. rewrite take_idem. auto.
  + remember (append_map_inf (delimit (S n) g1) (l :: take n s1)) as part1.
    simpl in Heqpart1. subst.
    remember (append_map_inf g1 (l :: s1)) as part1.
    simpl in Heqpart1. subst.
    repeat rewrite take_fold_right_app_comm.
    repeat rewrite take_one.
    rewrite IHn.
    remember (append_map_inf (delimit (S n) g1) (l :: take n s1)) as part1.
    simpl in Heqpart1. subst.
    rewrite take_fold_right_app_comm.
    rewrite take_one.
    assert (H1: take n (append_map_inf (delimit n g1) (take n s1)) =
                take n (append_map_inf (delimit (S n) g1) (take n s1))).
    {
      rewrite (IHn (delimit (S n) g1) (take n s1)).
      rewrite delimit_refine.
      rewrite take_idem. auto.
    }
    assert (H2: (map (take (S n)) (map g1 l)) =
                (map (take (S n)) (map (delimit (S n) g1) l))).
    { repeat rewrite map_map_compose.
      assert (H3: (fun x : state => take (S n) (g1 x)) =
                  (fun x : state => take (S n) (delimit (S n) g1 x))).
      { extensionality s.
        unfold delimit.
        rewrite take_idem. auto. }
      rewrite H3. auto. }
    rewrite H1. rewrite H2. auto.
Qed.

Theorem limit_mk_conj2 : forall n g1 g2,
  run n (mk_conj2 g1 g2) = run n (mk_conj2 (delimit n g1) (delimit n g2)).
Proof.
unfold run. unfold mk_conj2.
intros. extensionality s.
rewrite (limit_append_map_inf n g2 (g1 s)). auto.
Qed.

Theorem limit_seres_disj2: forall n g1 g2,
  run n (seres_disj2 g1 g2) = run n (seres_disj2 (delimit n g1) (delimit n g2)).
Proof.
unfold run. unfold delimit. unfold seres_disj2. 
intros. extensionality s.
rewrite <- append_inf_is_zipw_app. apply limit_append_inf.
Qed.

Theorem limit_seres_conj2: forall n g1 g2,
  run n (seres_conj2 g1 g2) = run n (seres_conj2 (delimit n g1) (delimit n g2)).
Proof. 
(* This proof looks not very appropriate, because conj_eqv is only applicable to
   finite streams. *)
pose conj2_eqv. unfold obs_eqv in o.
intros. repeat rewrite <- o. apply limit_mk_conj2.
Qed.
