#| Space × (State × Space → Space) × (→ Space) → Space |#
(define (elim s-inf fk sk)
  (let ((ss (car s-inf)) (f (cdr s-inf)))
    (cond
      ((pair? ss) (sk (car ss) (cons (cdr ss) f)))
      (f (step (lambda () (elim (f) fk sk))))
      (else (fk)))))

#| Goal × Goal × Goal → Goal |#
(define (ifte g1 g2 g3)
  (lambda (s)
    (elim (g1 s)
      (lambda () (g3 s))
      (lambda (s0 s-inf)
        (append-map-inf/fair g2
          (append-inf/fair (unit s0) s-inf))))))

#| Goal → Goal |#
(define (once g)
  (lambda (s)
    (elim (g s)
      (lambda () (none))
      (lambda (s0 s-inf) (unit s0)))))
