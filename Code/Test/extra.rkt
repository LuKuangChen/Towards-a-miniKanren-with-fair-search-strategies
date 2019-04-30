#lang racket
(require "../mk-4.rkt")

(defrel (recur== x y)
  (conde
    [(== x y)]
    [(recur== x y)]))

(run 50 q
  (conde
    [(recur== q 1)]
    [(recur== q 2)]
    [(recur== q 3)]
    [(recur== q 4)]
    [(recur== q 5)]))

(defrel (nevero)
  (nevero))

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))
#;
(run 30 (p q)
  (conde
    [(conde
       [(== q 1) (alwayso)]
       [(== q 2) (alwayso)]
       [(== q 3) (alwayso)])
     (== p 'foobar)]
    [(== `(,p ,q) '(a b)) (alwayso)]))

(defrel (natural^o n out)
  (project (n)
    (conde
      [(== n out)]
      [(fresh (m)
         (== m (add1 n))
         (natural^o m out))])))

(defrel (naturalo out)
  (fresh (n)
    (== n 0)
    (natural^o n out)))

(run 10 q
  (naturalo q))