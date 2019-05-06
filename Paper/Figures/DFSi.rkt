#| Goal x Goal -> Goal |#
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

#| Space x Space -> Space |#
(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
       (append-inf (cdr s-inf) t-inf)))
    (else (lambda () 
            (append-inf t-inf (s-inf))))))

#| Goal x Goal -> Goal |#
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

#| Goal x Space -> Space |#
(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))
