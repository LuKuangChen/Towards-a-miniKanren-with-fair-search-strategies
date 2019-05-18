(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj g ...) (disj+ (g ...) () ())]))

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