#lang racket
(require "../mk-0.rkt")

(defrel (lookupo env id val)
  (conde
    [(fresh (env^)
       (== 0 id)
       (== `(,val . ,env^) env))]
    [(fresh (id^ val^ env^)
       (== `(add1 ,id^) id)
       (== `(,val^ . ,env^) env)
       (lookupo env^ id^ val))]))

(defrel (valofo e env out)
  (conde
    [(fresh (x)
       (== `(quote ,x) e)
       (== `(quote ,x) out))]
    [(fresh (es os)
       (== `(list . ,es) e)
       (== `(quote ,os) out)
       (valof*o es env os))]
    [(fresh (id)
       (== `(var ,id) e)
       (lookupo env id out))]
    [(fresh (body)
       (== `(λ ,body) e)
       (== `(clos ,env ,body) out))]
    [(fresh (rator rand clos arg)
       (== `(,rator ,rand) e)
       (valofo rator env clos)
       (valofo rand env arg)
       (apply-closo clos arg out))]))

(defrel (valof*o es env os)
  (conde
    [(== '() es) (== '() os)]
    [(fresh (e es^ qo o os^)
       (== `(,e . ,es^) es)
       (== `(,o . ,os^) os)
       (valofo e env `(quote ,o))
       (valof*o es^ env os^))]))

(defrel (apply-closo clos arg out)
  (fresh (env body)
    (== `(clos ,env ,body) clos)
    (valofo body `(,arg . ,env) out)))

(defrel (evalo e out)
  (valofo e '() out))

;; query a quine
(run 1 q
  (evalo q `(quote ,q)))