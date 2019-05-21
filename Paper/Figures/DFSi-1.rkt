#| Goal × Goal → Goal |#
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

#| Goal × Space → Space |#
(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

(define-syntax conj
  (syntax-rules ()
    ((conj) (fail))
    ((conj g0 g ...) (conj+ g0 g ...))))

(define-syntax conj+
  (syntax-rules ()
    ((conj+ g) g)
    ((conj+ g0 g1 g ...) (conj2 g0 (conj+ g1 g ...)))))