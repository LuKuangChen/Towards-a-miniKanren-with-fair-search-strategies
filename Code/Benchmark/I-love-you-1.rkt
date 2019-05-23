#lang racket
(require "../mk-BFSimp-1.rkt")

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
    ((fresh (a av d dv)
       (== `(cons ,a ,d) exp)
       (== `(quote (,av . ,dv)) val)
       (eval-expo a env `(quote ,av))
       (eval-expo d env `(quote ,dv))))
    ((fresh (rator rand x body env^ a)
       (== `(app ,rator ,rand) exp)
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env a)
       (eval-expo body `(,a . ,env^) val)))
    ((fresh (pr av dv)
       (== `(car ,pr) exp)
       (== `(quote ,av) val)
       (eval-expo pr env `(quote (,av . ,dv)))))
    ((fresh (pr av dv)
       (== `(cdr ,pr) exp)
       (== `(quote ,dv) val)
       (eval-expo pr env `(quote (,av . ,dv)))))))

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


(define (just-time n)
  (void (time (run n q
                (evalo q '(I love you))))))

(begin
  (just-time 99)
  (just-time 198)
  (just-time 297))