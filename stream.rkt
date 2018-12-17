#lang racket
#| stream-ifte |#

(provide empty-inf
         mature-inf
         immature-inf
         append-inf
         append-map-inf
         take-inf
         ; ifte
         empty-inf?
         has-mature-inf?
         first-mature-inf
         force-inf)
(require "fast-append.rkt")

#| BEGIN list helpers |#
(define (null) '())

(define (car+cdr pr)
  (values (car pr) (cdr pr)))
#| END list helpers |#

#| BEGIN ifte & once |#

(define (empty-inf? s-inf)
  (and (null? (car s-inf))
       (@null? (cdr s-inf))))

(define (has-mature-inf? s-inf)
  (not (null? (car s-inf))))

(define (first-mature-inf s-inf)
  (let-values ([(v vs) (car+cdr (car s-inf))])
    v))

(define (force-inf s-inf)
  (let-values ([(th ths) (@car×cdr (cdr s-inf))])
    (append-inf (cons (car s-inf) ths) (th))))

#| END ifte & once |#

(define (empty-inf) (cons (null) (@null)))
(define (mature-inf v) (cons (list v) (@null)))
(define (immature-inf th) (cons (null) (@list th)))

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
           [(@null? ths) '()]
           [else
            (let-values ([(th rest-ths) (@car×cdr ths)])
              (take-inf n (append-inf (cons (null) rest-ths) (th))))]))]
      [else (let-values ([(v rest-vs) (car+cdr vs)])
              (cons v (loop (and n (sub1 n)) rest-vs)))])))

#| (State → Stream) × Stream → Stream |#
(define (append-map-inf g s-inf)
  (let outer-loop ((vs (car s-inf))) ;; foldr
    (cond
      [(null? vs) (cons (null)
                        (let inner-loop ([ths (cdr s-inf)])  ;; map
                          (cond
                            [(@null? ths) (@null)]
                            [else (let-values ([(th rest-ths) (@car×cdr ths)])
                                    (@++ (@list (lambda () (append-map-inf g (th))))
                                         (inner-loop rest-ths)))])))]
      [else (let-values ([(v rest-vs) (car+cdr vs)])
              (append-inf (g v) (outer-loop rest-vs)))])))