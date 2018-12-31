#lang racket
#|

a very inefficient, but obviously correct way

represent
  - (@null) with '()
  - (@list x) with `(,x)
  - (@append xs ys) with (append xs ys)

|#
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

(define (empty-inf? s-inf)
  (and (null? (car s-inf))
       (null? (cdr s-inf))))

(define (has-mature-inf? s-inf)
  (not (null? (car s-inf))))

(define (first-mature-inf s-inf)
  (car (car s-inf)))

; the mature part must be empty
(define (force-inf s-inf)
  (let ([ths (cdr s-inf)])
    (let ([th (car ths)])
      (append-inf (cons '() (cdr ths)) (th)))))

(define (empty-inf) (cons '() '()))
(define (mature-inf v) (cons (list v) '()))
(define (immature-inf th) (cons '() (list th)))

#| Stream × Stream → Stream |#
(define (append-inf s-inf t-inf)
  (cons (append (car s-inf) (car t-inf))
    (append (cdr s-inf) (cdr t-inf))))

#| Nat × Stream → (List X) |#
(define (take-inf n s-inf)
  (let loop ([n n] [vs (car s-inf)])
    (cond
      [(and n (zero? n)) '()]
      [(null? vs)
       (let ([ths (cdr s-inf)])
         (cond
           [(null? ths) '()]
           [else (take-inf n (force-inf s-inf))]))]
      [else
       (cons (car vs)
         (loop (and n (sub1 n)) (cdr vs)))])))

#| (State → Stream) × Stream → Stream |#
(define (append-map-inf g s-inf)
  (let outer ([vs (car s-inf)])
    (cond
      [(null? vs)
       (cons '()
         (let inner ([ths (cdr s-inf)])
           (cond
             [(null? ths) '()]
             [else (let ([th (car ths)])
                     (append
                       (list (lambda ()
                               (append-map-inf g (th))))
                       (inner (cdr ths))))])))]
      [else
       (append-inf (g (car vs)) (outer (cdr vs)))])))