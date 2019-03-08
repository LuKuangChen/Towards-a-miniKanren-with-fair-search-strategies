#lang racket
(require "../mk-3.rkt")

(defrel (recur== x y)
  (conde
    [(== x y)]
    [(recur== x y)]))

(defrel (nevero)
  (nevero))

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(run 30 (p q)
  (conde
    [(conde
       [(== q 1) (alwayso)]
       [(== q 2) (alwayso)]
       [(== q 3) (alwayso)])
     (== p 'foobar)]
    [(== `(,p ,q) '(a b)) (alwayso)]))
