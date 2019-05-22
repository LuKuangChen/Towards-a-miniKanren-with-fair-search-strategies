#| Goal × Goal → Goal |#
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf/fair (g1 s) (g2 s))))

#| Space × Space → Space |#
(define (append-inf/fair s-inf t-inf)
  (let loop ((s? #t) (s-inf s-inf) (t-inf t-inf))
    (cond
      ((null? s-inf) t-inf)
      ((pair? s-inf)
       (cons (car s-inf)
         (loop s? (cdr s-inf) t-inf)))
      (s? (loop #f t-inf s-inf))
      (else (lambda ()
              (loop #t (t-inf) (s-inf)))))))
