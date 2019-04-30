#lang racket
(require "../mk-silvija.rkt")

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
    [(fresh (id)
       (== `(var ,id) e)
       (lookupo env id out))]
    [(fresh (x)
       (== `(quote ,x) e)
       (== `(quote ,x) out))]
    [(fresh (body)
       (== `(lambda ,body) e)
       (== `(clos ,env ,body) out))]
    [(fresh (a d ao do)
       (== `(cons ,a ,d) e)
       (== `(cons ,ao ,do) out)
       (valofo a env ao)
       (valofo d env do))]
    [(fresh (rator rand cenv body arg)
       (== `(,rator ,rand) e)
       (valofo rator env `(clos ,cenv ,body))
       (valofo rand env arg)
       (valofo body `(,arg . ,cenv) out))]
    [(fresh (pr do)
       (== `(car ,pr) e)
       (valofo pr env `(cons ,out ,do)))]
    [(fresh (pr ao)
       (== `(cdr ,pr) e)
       (valofo pr env `(cons ,ao ,out)))]))

(defrel (observeo val out)
  (conde
    [(== `(quote ,out) val)]
    [(fresh (av dv ao do)
       (== `(cons ,av ,dv) val)
       (== `(,ao . ,do) out)
       (observeo av ao)
       (observeo dv do))]))

(defrel (evalo e out)
  (fresh (val)
    (observeo val out)
    (valofo e '() val)))

(define (just-time/♥ n)
  (void (time (run n q
                (evalo q '(I love you))))))

(begin
  (just-time/♥ 99)
  (just-time/♥ 199)
  (just-time/♥ 299))