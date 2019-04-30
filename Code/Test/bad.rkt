#lang racket
(require "../mk-3.rkt")

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(defrel (bado x)
  (conde
    [(== x 'pie)
     (alwayso)]
    [(== x 'pie)
     (alwayso)]
    [(== x 'pie)
     (alwayso)]
    [(== x 'pie)
     (alwayso)]
    [(== x 'pie)
     (alwayso)]
    [(== x 'good)
     (alwayso)]))

(length (run 1000000 q
          (bado q)
          (== q 'good)))