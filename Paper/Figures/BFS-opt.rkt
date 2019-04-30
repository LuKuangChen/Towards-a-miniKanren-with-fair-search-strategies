(define (none)   `(()   . #f))
(define (unit s) `((,s) . #f))
(define (step f) `(()   . ,f))

(define (split s-inf ks kf)
  (let ([ss (car s-inf)]
        [f (cdr s-inf)])
    (cond
      [(and (null? ss) f)
       (step (lambda () (split (f) ks kf)))]
      [(null? ss) (kf)]
      [else (ks (car ss) (cons (cdr ss) f))])))

(define (append-inf/fair s-inf t-inf)
  (cons (append (car s-inf) (car t-inf))
    (let ([t1 (cdr s-inf)]
          [t2 (cdr t-inf)])
      (cond
        [(not t1) t2]
        [(not t2) t1]
        [else (lambda () (append-inf/fair (t1) (t2)))]))))

(define (append-map-inf g s-inf)
  (foldr
    (lambda (s t-inf)
      (append-inf/fair (g s) t-inf))
    (let ([f (cdr s-inf)])
      (step (and f (lambda () (append-map-inf g (f))))))
    (car s-inf)))

(define (take-inf n s-inf)
  (let loop ([n n]
             [vs (car s-inf)])
    (cond
      ((and n (zero? n)) '())
      ((pair? vs)
       (cons (car vs)
         (loop (and n (sub1 n)) (cdr vs))))
      (else
       (let ([f (cdr s-inf)])
         (if f (take-inf n (f)) '()))))))