#lang racket
(require "../mk-all-in-one.rkt")


(defrel (repeato x out)
  (conde
    [(== `(,x) out)]
    [(fresh (res)
       (== `(,x . ,res) out)
       (repeato x res))]))

#;
(run BFS 12 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]))
#;
(run BFS 12 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]
    [(repeato 'd q)]))

(run BFS 16 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]
    [(repeato 'd q)]
    [(repeato 'e q)]))

(run BFS 12 q
  (fresh (x)
    (conde
      [(== 'a x)]
      [(== 'b x)]
      [(== 'c x)]
      [(== 'd x)])
    (repeato x q)))
#;
(run BFS 12 q
  (fresh (x)
    (repeato x q)
    (conde
      [(== 'a x)]
      [(== 'b x)]
      [(== 'c x)]
      [(== 'd x)])))


(run BFS 12 q
  (fresh (xs)
    (conde
      [(repeato 'a xs)]
      [(repeato 'b xs)])
    (repeato xs q)))