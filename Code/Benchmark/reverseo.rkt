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

(defrel (reverso l out)
  (conde
    [(nullo l) (nullo out)]
    [(fresh (a d res)
       (conso a d l)
       (appendo res `(,a) out)
       (reverso d res))]))

(define (just-time n)
  (void (time (run n (p q)
                (reverso p q)))))

(just-time 10)
(just-time 20)
(just-time 30)