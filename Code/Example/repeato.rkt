#lang racket
(require "../mk-iDFS.rkt")


(defrel (repeato x out)
  (conde
    [(== `(,x) out)]
    [(fresh (res)
       (== `(,x . ,res) out)
       (repeato x res))]))

#;
(run 12 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]))
#;
(run 12 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]
    [(repeato 'd q)]))
#;
(run 16 q
  (conde
    [(repeato 'a q)]
    [(repeato 'b q)]
    [(repeato 'c q)]
    [(repeato 'd q)]
    [(repeato 'e q)]))

(run 12 q
  (fresh (x)
    (conde
      [(== 'a x)]
      [(== 'b x)]
      [(== 'c x)])
    (repeato x q)))
#;
(run 12 q
  (fresh (x)
    (repeato x q)
    (conde
      [(== 'a x)]
      [(== 'b x)]
      [(== 'c x)]
      [(== 'd x)])))

#;
(run 12 q
  (fresh (xs)
    (conde
      [(repeato 'a xs)]
      [(repeato 'b xs)])
    (repeato xs q)))