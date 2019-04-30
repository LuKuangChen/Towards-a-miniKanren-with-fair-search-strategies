#lang racket
(require "../mk-BFS-opt.rkt")

(defrel (appendo l r o)
  (conde
    [(== '() l) (== r o)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) o)
       (appendo d r res))]))

(defrel (reverseo l o)
  (conde
    [(== '() l) (== '() o)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (appendo res `(,a) o)
       (reverseo d res))]))

(define (just-time n)
  (void (time (run n (p q)
                (reverseo p q)))))

(just-time 10)
(just-time 20)
(just-time 30)