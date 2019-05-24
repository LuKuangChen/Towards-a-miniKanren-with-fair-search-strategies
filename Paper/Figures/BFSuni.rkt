#| Goal × Space → Space |#
(define (append-map-inf/fair g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf/fair (g (car s-inf))
       (append-map-inf/fair g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf/fair g (s-inf))))))