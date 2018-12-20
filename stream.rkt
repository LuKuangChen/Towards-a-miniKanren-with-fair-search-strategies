#lang racket
(provide empty-inf
         mature-inf
         immature-inf
         append-inf
         append-map-inf
         take-inf
         empty-inf?
         has-mature-inf?
         first-mature-inf
         force-inf)

#|

@List ::= '() | @NonEmpty
@NotEmpty ::= `(,X .()) | `(,@NotEmpty . ,@NotEmpty)

|#

#| @List × @List → @List |#
(define (@++ x y)
  (cond
    [(null? x) y]
    [(null? y) x]
    [else (cons x y)]))

#| @List → X × @List |#
(define (@car×cdr x)
  (let ([a (car x)]
        [d (cdr x)])
    (cond
      [(null? d) (values a '())]
      [else
       (let loop ([a (car a)] [d (cdr a)] [acc d])
         (cond
           [(null? d) (values a acc)]
           [else (loop (car a) (cdr a) (cons d acc))]))])))

(define (empty-inf? s-inf)
  (and (null? (car s-inf))
       (null? (cdr s-inf))))

(define (has-mature-inf? s-inf)
  (not (null? (car s-inf))))

(define (first-mature-inf s-inf)
  (car (car s-inf)))

(define (force-inf s-inf)
  (let-values ([(th ths) (@car×cdr (cdr s-inf))])
    (append-inf (cons (car s-inf) ths) (th))))

(define (empty-inf) (cons '() '()))
(define (mature-inf v) (cons (list v) '()))
(define (immature-inf th) (cons '() (list th)))

#| Stream × Stream → Stream |#
(define (append-inf s-inf t-inf)
  (cons (append (car s-inf) (car t-inf))
        (@++ (cdr s-inf) (cdr t-inf))))

#| Nat × Stream → (List X) |#
(define (take-inf n s-inf)
  (let loop ([n n] [vs (car s-inf)])
    (cond
      [(and n (zero? n)) '()]
      [(null? vs)
       (let ([ths (cdr s-inf)])
         (cond
           [(null? ths) '()]
           [else
            (let-values ([(th rest-ths) (@car×cdr ths)])
              (take-inf n (append-inf (cons '() rest-ths) (th))))]))]
      [else (cons (car vs) (loop (and n (sub1 n)) (cdr vs)))])))

#| (State → Stream) × Stream → Stream |#
(define (append-map-inf g s-inf)
  (let outer ((vs (car s-inf)))
    (cond
      [(null? vs)
       (cons '() (let inner ([ths (cdr s-inf)])
                   (cond
                     [(null? ths) '()]
                     [else (let-values ([(th rest-ths) (@car×cdr ths)])
                             (@++ (list (lambda () (append-map-inf g (th))))
                                  (inner rest-ths)))])))]
      [else (append-inf (g (car vs)) (outer (cdr vs)))])))