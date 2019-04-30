#lang racket
(require "../mk-5.scm")

(defrel (lookup x vars vals o)
  (fresh (y vars^ v vals^)
    (== `(,y . ,vars^) vars)
    (== `(,v . ,vals^) vals)
    (conde
      [(== x y) (== v o)]
      [(lookup x vars^ vals^ o)])))

(defrel (valsof es vars vals o)
  (conde 
    [(== `() es) (== '() o)]
    [(fresh (e es^)
       (== `(,e . ,es^) es)
       (fresh (v vs)
         (== `(,v . ,vs) o)
         (valof e vars vals v)
         (valsof es^ vars vals vs)))]))

(defrel (valof exp vars vals o)
  (conde
    [(lookup exp vars vals o)]
    [(fresh (x b)
       (== `(λ (,x) ,b) exp)
       (== `(clos ,x ,b ,vars ,vals) o))]
    [(== `(quote ,o) exp)]
    [(fresh (es)
       (== `(list . ,es) exp)
       (valsof es vars vals o))]
    [(fresh (rator rand)
       (== `(,rator ,rand) exp)
       (fresh (x b vars^ vals^ a) 
         (valof rator vars vals `(clos ,x ,b ,vars^ ,vals^))
         (valof rand vars vals a)
         (valof b `(,x . ,vars^) `(,a . ,vals^) o)))]))

(run 1 q
  (valof q '() '() q))