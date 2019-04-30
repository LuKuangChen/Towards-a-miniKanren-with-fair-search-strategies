#lang racket
(require "../mk-3-1.rkt")

(defrel (naturalo-helper n out)
  (project (n)
    (conde
      [(== n out)]
      [(fresh (m)
         (== m (add1 n))
         (naturalo-helper m out))])))

(defrel (naturalo out)
  (fresh (n)
    (== n 0)
    (naturalo-helper n out)))

(run 10 q
  (naturalo q))