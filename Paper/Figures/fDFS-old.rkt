#| Goal prod Goal -> Goal |#
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf/fair (g1 s) (g2 s))))

#| Space prod Space -> Space |#
(define (append-inf/fair s-inf t-inf)
  (append-inf/fair^ #t s-inf t-inf))

#| Bool prod Space prod Space -> Space |#
(define (append-inf/fair^ s? s-inf t-inf)
  (cond
    ((pair? s-inf)
     (cons (car s-inf)
       (append-inf/fair^ s? (cdr s-inf) t-inf)))
    ((null? s-inf) t-inf)
    (s? (append-inf/fair^ #f t-inf s-inf))
    (else (lambda ()
            (append-inf/fair (t-inf) (s-inf))))))