(define (append-map-inf g s-inf)
  (foldr append-inf
         (cons '()
           (map (lambda (t)
                  (lambda () (append-map-inf g (t))))
                (cdr s-inf)))
         (map g (car s-inf))))