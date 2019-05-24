#lang racket
(provide (all-defined-out))
#| mk-silbija |#

; 1 of 8 changes
(define (empty-inf) '())
(define (unit x) `((,x)))
(define (step s-inf) `(() . ,s-inf))

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

(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s (unit s) (empty-inf)))))

(define succeed
  (lambda (s)
    (unit s)))
 
(define fail
  (lambda (s)
    (empty-inf)))

; 2 of 8 changes
(define (disj2 g1 g2)
  (lambda (s)
    (zipw append (g1 s) (g2 s))))

; 3 of 8 changes
(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null? s-inf) '())
    ((and (pair? s-inf) (pair? (car s-inf)))
     (cons (car (car s-inf))
       (take-inf (and n (sub1 n))
         (cons (cdr (car s-inf)) (cdr s-inf)))))
    ((pair? s-inf) (take-inf n (cdr s-inf)))
    (else (take-inf n (s-inf)))))

; 4 of 8 changes
(define (append-map-inf g s-inf)
  (shuffle ((mmap g) s-inf)))

; 5 of 8 changes (including the following 8 helpers)
(define ((conj2 p1 p2) s)
  (shuffle ((mmap p2) (p1 s))))

; conj2 helper
(define (smap f sx)
  (cond
    ((null? sx) '())
    ((pair? sx) (cons (f (car sx)) (smap f (cdr sx))))
    (else (lambda () (smap f (sx))))))

; conj2 helper
(define (mmap f)
  (lambda (slx)
    (smap (lambda (xs) (map f xs)) slx)))

; conj2 helper
(define (concat xss)
  (foldr append '() xss))

; conj2 helper
(define (shuffle slslx)
  (smap (compose concat concat)
        (diag (smap transpose slslx))))

; conj2 helper
(define (wrap x) `(,x))

; conj2 helper
(define (diag ssx)
  (cond
    ((null? ssx) '())
    ((and (pair? ssx) (null? (car ssx)))
     (cons '() (diag (cdr ssx))))
    ((pair? ssx)
     (cons `(,(car (car ssx)))
       (zipw append
             (smap wrap (cdr (car ssx)))
             (diag (cdr ssx)))))
    (else (lambda () (diag (ssx))))))

; conj2 helper
(define (transpose lsx)
  (cond
    ((null? lsx) '())
    (else (zipw append
                (smap wrap (car lsx))
                (transpose (cdr lsx))))))

; conj2 helper
(define (zipw f sx sy)
  (cond
    ((null? sx) sy)
    ((null? sy) sx)
    ((and (pair? sx) (pair? sy))
     (cons (f (car sx) (car sy))
       (zipw f (cdr sx) (cdr sy))))
    ((pair? sx) (lambda () (zipw f sx (sy))))
    (else (lambda () (zipw f (sx) sy)))))

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

; 6 of 8 changes
(define (ifte g1 g2 g3)
  (lambda (s)
    (let loop ((s-inf (g1 s)))
      (cond
        ((null? s-inf) (g3 s))
        ((and (pair? s-inf) (pair? (car s-inf)))
         (append-map-inf g2 s-inf))
        ((pair? s-inf) (step (loop (cdr s-inf))))
        (else (lambda () (loop (s-inf))))))))

; 7 of 8 changes
(define (once g)
  (lambda (s)
    (let loop ((s-inf (g s)))
      (cond
        ((null? s-inf) '())
        ((and (pair? s-inf) (pair? (car s-inf)))
         (unit (car (car s-inf))))
        ((pair? s-inf) (step (loop (cdr s-inf))))
        (else (lambda () (loop (s-inf))))))))

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

; 8 of 8 changes
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (step
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
