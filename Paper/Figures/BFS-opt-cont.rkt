(define (elim s-inf ks kf)
  (let ((ss (car s-inf))
        (f (cdr s-inf)))
    (cond
      ((and (null? ss) f)
       (step (lambda () (elim (f) ks kf))))
      ((null? ss) (kf))
      (else (ks (car ss) (cons (cdr ss) f))))))

(define (ifte g1 g2 g3)
  (lambda (s)
    (elim (g1 s)
      (lambda (s0 s-inf)
        (append-map-inf/fair g2
          (append-inf/fair (unit s0) s-inf)))
      (lambda () (g3 s)))))

(define (once g)
  (lambda (s)
    (elim (g s)
      (lambda (s0 s-inf) (unit s0))
      (lambda () (none)))))