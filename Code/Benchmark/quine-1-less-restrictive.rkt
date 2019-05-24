#lang racket
(require "../mk-DFSi.rkt")

(defrel (evalo exp out)
  (fresh (val)
    (eval-expo exp '() val)
    (observeo val out)))

(defrel (observeo val out)
  (conde
    ((== `(quote ,out) val))
    ((fresh (vs)
       (== `(list . ,vs) val)
       (observe*o vs out)))))

(defrel (observe*o vals outs)
  (conde
    ((== '() vals)
     (== '() outs))
    ((fresh (v0 o0 v* o*)
       (== `(,v0 . ,v*) vals)
       (== `(,o0 . ,o*) outs)
       (observeo v0 o0)
       (observe*o v* o*)))))

(defrel (eval-expo exp env val)
  (conde
    ((fresh (var)
       (== `(var ,var) exp)
       (lookupo var env val)))
    ((fresh (v)
       (== `(quote ,v) exp)
       (== `(quote ,v) val)))
    ((fresh (x body)
       (== `(lambda ,body) exp)
       (== `(closure ,body ,env) val)))
    ((fresh (exps vals)
       (== `(list . ,exps) exp)
       (== `(list . ,vals) val)
       (proper-listo exps env vals)))
    ((fresh (rator rand x body env^ a)
       (== `(app ,rator ,rand) exp)
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env a)
       (eval-expo body `(,a . ,env^) val)))))

(defrel (proper-listo exps env vals)
  (conde
    ((== '() exps)
     (== '() vals))
    ((fresh (e0 e* v0 v*)
       (== `(,e0 . ,e*) exps)
       (== `(,v0 . ,v*) vals)
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
                (evalo q q)))))

(custodian-limit-memory
 (current-custodian)
 (* 500 1024 1024))

(begin
  (just-time 1)
  (just-time 2)
  (just-time 3))