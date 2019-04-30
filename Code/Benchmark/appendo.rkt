#lang racket
(require "../mk-BFS-opt.rkt")

(defrel (appendo l r o)
  (conde
    [(== '() l) (== r o)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) o)
       (appendo d r res))]))

(define (just-time n)
  (void (time (run n (p q r)
                (appendo p q r)))))

(just-time 100)
(just-time 200)
(just-time 300)