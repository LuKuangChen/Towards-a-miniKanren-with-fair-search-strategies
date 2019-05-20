#lang racket

(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj g ...) (disj+ (g ...) () ())]))

;; disj+ recurs on the first argument
;; The next two arguments are accumulators
(define-syntax disj+
  (syntax-rules ()
    [(disj+ (g) () ()) g]
    [(disj+ () (gl ...) (gr ...))
     (disj2 (disj+ (gl ...) () ())
            (disj+ (gr ...) () ()))]
    [(disj+ (g0) (gl ...) (gr ...))
     (disj2 (disj+ (gl ... g0) () ())
            (disj+ (gr ...) () ()))]
    [(disj+ (g0 g1 g ...) (gl ...) (gr ...))
     (disj+ (g ...) (gl ... g0) (gr ... g1))]))

;; For testing purpose ...
(define (disj2 g1 g2) `(,g1 ,g2))
;; > (disj 1 2 3 4 5)
;; '(((1 5) 3) (2 4))
