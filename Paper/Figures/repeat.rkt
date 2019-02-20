#lang racket
(require "mk-bfs.rkt")

(defrel (repeato x out)
  (conde
    [(≡ '() out)]
    [(fresh (res)
       (≡ `(,x . ,res) out)
       (repeato x res))]))

(run 4 q
  (repeato '* q))
;; -->
;; '(() (*) (* *) (* * *))