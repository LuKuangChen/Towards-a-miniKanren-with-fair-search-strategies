#lang racket
(require "../mk-BFSimp-1.rkt")

(defrel (nevero)
  (nevero))

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(defrel (very-recursiveo)
  (conde
    [(nevero)]
    [(very-recursiveo)]
    [(alwayso)]
    [(very-recursiveo)]
    [(nevero)]))

(define (just-time n)
  (void (time (run n q
                (very-recursiveo)))))

(begin
  (just-time 100000)
  (just-time 200000)
  (just-time 300000))