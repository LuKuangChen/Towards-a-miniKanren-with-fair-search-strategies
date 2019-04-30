#lang racket
(require "../mk-0.rkt")

(define counter (box 0))

(defrel (repeato x xs)
  (conde
    [(== xs '())]
    [(fresh (d)
       (== xs `(,x . ,d))
       (repeato x d))]))

(defrel (relo x y)
  (project (x y)
    (begin
      (set-box! counter (add1 (unbox counter)))
      (displayln (list counter x y))
      (conde
        [(== x '()) (== y 'yeah)]
        [(fresh (a d)
           (== x `(,a . ,d))
           (relo d y))]))))

(run 3 (p q)
  (repeato 'x q)
  (project (q)
    (begin (displayln q)
           (relo q p))))