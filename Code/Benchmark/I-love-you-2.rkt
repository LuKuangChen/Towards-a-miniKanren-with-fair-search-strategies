#lang racket
(require "../mk-sBFS.rkt")

(defrel (evalo exp out)
  (fresh (val)
    (observeo val out)
    (eval-expo exp '() val)))

(defrel (observeo val out)
  (conde
    ((== `(quote ,out) val))
    ((fresh (av dv ao do)
       (== `(cons ,av ,dv) val)
       (== `(,ao . ,do) out)
       (observeo av ao)
       (observeo dv do)))))

(defrel (eval-expo exp env val)
  (conde
    ((fresh (rator rand x body env^ a)
       (== `(app ,rator ,rand) exp)
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env a)
       (eval-expo body `(,a . ,env^) val)))
    ((fresh (pr vd)
       (== `(car ,pr) exp)
       (eval-expo pr env `(cons ,val ,vd))))
    ((fresh (pr va)
       (== `(cdr ,pr) exp)
       (eval-expo pr env `(cons ,va ,val))))
    ((fresh (var)
       (== `(var ,var) exp)
       (lookupo var env val)))
    ((fresh (v)
       (== `(quote ,v) exp)
       (== `(quote ,v) val)))
    ((fresh (x body)
       (== `(lambda ,body) exp)
       (== `(closure ,body ,env) val)))
    ((fresh (ea va ed vd)
       (== `(cons ,ea ,ed) exp)
       (== `(cons ,va ,vd) val)
       (eval-expo ea env va)
       (eval-expo ed env vd)))))

(defrel (proper-listo exps env val)
  (conde
    ((== '() exps)
     (== '(quote ()) val))
    ((fresh (e0 e* v0 v*)
       (== `(,e0 . ,e*) exps)
       (== `(cons ,v0 ,v*) val)
       (eval-expo e0 env v0)
       (proper-listo e* env v*)))))

(defrel (lookupo x env t)
  (fresh (rest y v)
    (== `(,v . ,rest) env)
    (conde
      ((== 0 x) (== v t))
      ((== `(add1 ,y) x) (lookupo y rest t)))))

(define (just-time n)
  (void (time (run n q
                (evalo q '(I love you))))))
(custodian-limit-memory
 (current-custodian)
 (* 500 1024 1024))
(begin
  (just-time 99)
  (just-time 198)
  (just-time 297))