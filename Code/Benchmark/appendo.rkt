#lang racket
(require "../mk-BFS-opt.rkt")

(defrel (nullo x)
  (== '() x))

(defrel (conso a d p)
  (== `(,a . ,d) p))

(defrel (appendo l t out)
  (conde
    [(nullo l) (== t out)]
    [(fresh (a d res)
       (conso a d l)
       (conso a res out)
       (appendo d t res))]))

(define (just-time n)
  (void (time (run n (p q r)
                (appendo p q r)))))

(just-time 100)
(just-time 200)
(just-time 300)