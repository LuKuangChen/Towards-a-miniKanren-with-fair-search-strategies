#lang racket
(require "../mk-sBFS.rkt")

(defrel (evalo exp val)
  (eval-expo exp '() `(quote ,val)))

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
    ((fresh (a* ls)
       (== `(list . ,a*) exp)
       (== `(quote ,ls) val)
       (proper-listo a* env ls)))
    ((fresh (rator rand x body env^ a)
       (== `(app ,rator ,rand) exp)
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env a)
       (eval-expo body `(,a . ,env^) val)))))

(defrel (proper-listo exp env val)
  (conde
    ((== '() exp)
     (== '() val))
    ((fresh (a d t-a t-d)
       (== `(,a . ,d) exp)
       (== `(,t-a . ,t-d) val)
       (eval-expo a env `(quote ,t-a))
       (proper-listo d env t-d)))))

(defrel (lookupo x env t)
  (fresh (rest y v)
    (== `(,v . ,rest) env)
    (conde
      ((== 0 x) (== v t))
      ((== `(add1 ,y) x) (lookupo y rest t)))))


(define (just-time/quine n)
  (void (time (run n q
                (evalo q q)))))

(custodian-limit-memory
 (current-custodian)
 (* 500 1024 1024))
(begin
  (just-time/quine 0)
  (just-time/quine 1)
  (just-time/quine 2)
  (just-time/quine 3))