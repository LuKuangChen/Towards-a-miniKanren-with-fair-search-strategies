#| Goal × Goal → Goal |#
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

#| Space × Space → Space |#
(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
       (append-inf (cdr s-inf) t-inf)))
    (else (lambda () 
            (append-inf t-inf (s-inf))))))

(define-syntax disj
  (syntax-rules ()
    ((disj) (fail))
    ((disj g0 g ...) (disj+ g0 g ...))))

(define-syntax disj+
  (syntax-rules ()
    ((disj+ g) g)
    ((disj+ g0 g1 g ...) (disj2 g0 (disj+ g1 g ...)))))