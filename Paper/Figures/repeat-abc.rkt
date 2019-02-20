#lang racket
(require "mk-0.rkt")

(defrel (repeato x out)
  (conde
    [(== '() out)]
    [(fresh (res)
       (== `(,x . ,res) out)
       (repeato x res))]))

(run 12 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]))
;; --> [BFS]
;; '(() () ()
;;   (a) (b) (c)
;;   (a a) (b b) (c c)
;;   (a a a) (b b b) (c c c))
;; --> [interleaving DFS]
;; '(() (a) ()
;;   (a a) () (a a a)
;;   (b) (a a a a) (c)
;;   (a a a a a) (b b) (a a a a a a))