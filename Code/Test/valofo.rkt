#lang racket
(require "../mk-0.rkt")

(defrel (lookupo env n val)
  (conde
    [(fresh (env^)
       (== 0 n)
       (== `(,val . ,env^) env))]
    [(fresh (m val^ env^)
       (== `(add1 ,m) n)
       (== `(,val^ . ,env^) env)
       (lookupo env^ m val))]))

(defrel (valofo e env out)
  (conde
    [(fresh (x)
       (== `(quote ,x) e)
       (== `(quote ,x) out))]
    [(fresh (a d ao do)
       (== `(cons ,a ,d) e)
       (== `(quote (,ao . ,do)) out)
       (valofo a env `(quote ,ao))
       (valofo d env `(quote ,do)))]
    [(fresh (pr ao do)
       (== `(car ,pr) e)
       (== `(quote ,ao) out)
       (valofo pr env `(quote (,ao . ,do))))]
    [(fresh (pr ao do)
       (== `(cdr ,pr) e)
       (== `(quote ,do) out)
       (valofo pr env `(quote (,ao . ,do))))]
    [(fresh (id)
       (== `(var ,id) e)
       (lookupo env id out))]
    [(fresh (body)
       (== `(lambda ,body) e)
       (== `(clos ,env ,body) out))]
    [(fresh (rator rand cenv body arg)
       (== `(,rator ,rand) e)
       (valofo rator env `(clos ,cenv ,body))
       (valofo rand env arg)
       (valofo body `(,arg . ,cenv) out))]))

(defrel (evalo e out)
  (valofo e '() out))

#;
(length
 (run 9999 q
   (evalo q '(quote (I love you)))))

(run 1 q
  (evalo q `(quote ,q)))