#lang racket
(provide (all-defined-out))
#| mk-2 |#
#|

* RI search space
* use "cons" (instead of "append") to combine mature
  part and immature part of search space

|#

; 1/7 CHANGES
(define (empty-inf) '(() . ()))
(define (unit-mature-inf v) `((,v) . ()))
(define (unit-immature-inf th) `(() . ,th))

; 2/7 CHANGES
(define (null-inf? s-inf)
  (and (null? (car s-inf))
       (null? (cdr s-inf))))

; 3/7 CHANGES
; Ask whether some answer is ready to use
(define (mature-inf? s-inf)
  (pair? (car s-inf)))

; 4/7 CHANGES
; Take the first answer of a mature-inf
(define (car-inf s-inf)
  (car (car s-inf)))

; 5/7 CHANGES
; Drop the first answer of a mature-inf
(define (cdr-inf s-inf)
  (cons (cdr (car s-inf))
    (cdr s-inf)))

; 6/7 CHANGES
; Try to get more answer from an ∞ which is neither
; null-inf nor mature-inf
(define (force-inf s-inf)
  (let ((th (cdr s-inf)))
    (th)))

#| code below is RI wrt ∞, except append-map |#

(define var (lambda (x) (vector x)))
(define var? (lambda (x) (vector? x)))

(define empty-s '())

(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (cond
      ((pair? a) (walk (cdr a) s))
      (else v))))

(define (ext-s x v s)
  (cond
    ((occurs? x v s) #f)
    (else (cons `(,x . ,v) s))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eqv? v x))
      ((pair? v) 
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else #f))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((eqv? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s
           (unify (cdr u) (cdr v) s))))
      (else #f))))

; trivial change
(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s (unit-mature-inf s) (empty-inf)))))

; trivial change
(define succeed
  (lambda (s)
    (unit-mature-inf s)))
 
; trivial change
(define fail
  (lambda (s)
    (empty-inf)))

(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

; 7/7 CHANGES
; Do the same thing as the previous version.
(define (append-inf s-inf t-inf)
  (cons (append (car s-inf) (car t-inf))
    (let ((s (cdr s-inf))
          (t (cdr t-inf)))
      (cond
        ((null? s) t)
        ((null? t) s)
        (else (lambda () (append-inf (s) (t))))))))

; trivial change
(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null-inf? s-inf) '())
    ((mature-inf? s-inf)
     (cons (car-inf s-inf)
       (take-inf (and n (sub1 n))
         (cdr-inf s-inf))))
    (else (take-inf n (force-inf s-inf)))))

(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

; trivial change
(define (append-map-inf g s-inf)
  (cond
    ((null-inf? s-inf) (empty-inf))
    ((mature-inf? s-inf)
     (append-inf (g (car-inf s-inf))
       (append-map-inf g (cdr-inf s-inf))))
    (else (unit-immature-inf
            (lambda ()
              (append-map-inf g (force-inf s-inf)))))))

(define (call/fresh name f)
  (f (var name)))

(define (reify-name n)
  (string->symbol
    (string-append "_"
      (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons
         (walk* (car v) s)
         (walk* (cdr v) s)))
      (else v))))
#|
; 'project' is defined in the frame 10:98 on page 166.
(define-syntax project
  (syntax-rules ()
    ((project (x ...) g ...)
     (lambda (s)
       (let ((x (walk* x s)) ...)
         ((conj g ...) s))))))
|#
(define (reify-s v r)
  (let ((v (walk v r)))
    (cond
      ((var? v)
       (let ((n (length r)))
         (let ((rn (reify-name n)))
           (cons `(,v . ,rn) r))))
      ((pair? v)
       (let ((r (reify-s (car v) r)))
         (reify-s (cdr v) r)))
      (else r))))

(define (reify v)
  (lambda (s)
    (let ((v (walk* v s)))
      (let ((r (reify-s v empty-s)))
        (walk* v r)))))

(define (run-goal n g)
  (take-inf n (g empty-s)))

; trivial change
(define (ifte g1 g2 g3)
  (lambda (s)
    (let loop ((s-inf (g1 s)))
      (cond
        ((null-inf? s-inf) (g3 s))
        ((mature-inf? s-inf)
         (append-map-inf g2 s-inf))
        (else (unit-immature-inf
                (lambda ()
                  (loop (force-inf s-inf)))))))))

; trivial change
(define (once g)
  (lambda (s)
    (let loop ((s-inf (g s)))
      (cond
        ((null-inf? s-inf) (empty-inf))
        ((mature-inf? s-inf)
         (unit-mature-inf (car-inf s-inf)))
        (else (unit-immature-inf
                (lambda ()
                  (loop (force-inf s-inf)))))))))



;;; Here are the key parts of Appendix A

(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

; trivial change
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (unit-immature-inf
           (lambda ()
             ((conj g ...) s))))))))

(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
         (run-goal n (conj g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))

(define-syntax fresh
  (syntax-rules ()
    ((fresh () g ...) (conj g ...))
    ((fresh (x0 x ...) g ...)
     (call/fresh 'x_0
       (lambda (x0)
         (fresh (x ...) g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

(define-syntax conda
  (syntax-rules ()
    ((conda (g0 g ...)) (conj g0 g ...))
    ((conda (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (conda ln ...)))))

(define-syntax condu
  (syntax-rules ()
    ((condu (g0 g ...) ...)
     (conda ((once g0) g ...) ...))))
