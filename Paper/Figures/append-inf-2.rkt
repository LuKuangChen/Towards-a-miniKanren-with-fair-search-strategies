(define (append-inf s-inf t-inf)
  (cons (append (car s-inf) (car t-inf))
    (let ((s (cdr s-inf))
          (t (cdr t-inf)))
      (cond
        ((null? s) t)
        ((null? t) s)
        (else (lambda () (append-inf (s) (t))))))))