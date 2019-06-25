#lang racket
;(require "./mk-balanced-disj.rkt")
(require "./mk-0.rkt")

(defrel (nevero)
  (nevero))
(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(length
 (run 10000000 q
   (conde
     [(nevero)]
     [(nevero)]
     [(nevero)]
     [(nevero)]
     [(nevero)]
     [(alwayso)])))