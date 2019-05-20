Inductive stream (X : Type) :  Type :=
  | nil : stream X
  | cons : X -> stream X -> stream X
  | step : stream X -> stream X.

Arguments nil {X}.
Arguments cons {X} _ _.
Arguments step {X} _.

Definition unit {X} (a : X) : stream X := cons a nil.
Definition mzero {X} := @nil X.

Fixpoint mplus {X} s1 s2 : stream X :=
  match s1, s2 with
  | cons a s1', s2 => cons a (mplus s1' s2)
  | s1, cons a s2' => cons a (mplus s1 s2')
  | step s1', step s2' => step (mplus s1' s2')
  | nil, s2 => s2
  | s1, nil => s1
  end.