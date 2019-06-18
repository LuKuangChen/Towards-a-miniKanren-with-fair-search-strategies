#lang racket
(provide (all-defined-out))
#| our improved BFS |#

(define (unit s) `((,s) . #f))
(define (none)   `(()   . #f))
(define (step f) `(()   . ,f))

(define (elim s-inf kf ks)
  (let ((ss (car s-inf)) (f (cdr s-inf)))
    (cond
      ((pair? ss) (ks (car ss) (cons (cdr ss) f)))
      (f (step (lambda () (elim (f) kf ks))))
      (else (kf)))))

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
      (if s (unit s) (none)))))

(define succeed
  (lambda (s) (unit s)))
 
(define fail
  (lambda (s) (none)))

(define (disj2 g1 g2)
  (lambda (s)
    (append-inf/fair (g1 s) (g2 s))))

; 1/2 CHANGES
(define (append-inf/fair s-inf t-inf)
  (cons (append (car s-inf) (car t-inf))
    (let ([t1 (cdr s-inf)]
          [t2 (cdr t-inf)])
      (cond
        [(not t1) t2]
        [(not t2) t1]
        [else (lambda () (append-inf/fair (t1) (t2)))]))))

(define (take-inf n s-inf)
  (let loop ([n n]
             [ss (car s-inf)])
    (cond
      ((and n (zero? n)) '())
      ((pair? ss)
       (cons (car ss)
         (loop (and n (sub1 n)) (cdr ss))))
      (else
       (let ([f (cdr s-inf)])
         (if f (take-inf n (f)) '()))))))

(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf/fair g2 (g1 s))))

(define (append-map-inf/fair g s-inf)
  (foldr
    (lambda (s t-inf)
      (append-inf/fair (g s) t-inf))
    (let ([f (cdr s-inf)])
      (step (and f
              (lambda () (append-map-inf/fair g (f))))))
    (car s-inf)))

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

; 'project' is defined in the frame 10:98 on page 166.
(define-syntax project
  (syntax-rules ()
    ((project (x ...) g ...)
     (lambda (s)
       (let ((x (walk* x s)) ...)
         ((conj g ...) s))))))

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

;(define (ifte g1 g2 g3)
;  (lambda (s)
;    (let loop ((s-inf (g1 s)))
;      (cond
;        ((null? s-inf) (g3 s))
;        ((pair? s-inf)
;         (append-map-inf g2 s-inf))
;        (else (lambda ()
;                (loop (s-inf))))))))

(define (ifte g1 g2 g3)
  (lambda (s)
    (elim (g1 s)
      (lambda () (g3 s))
      (lambda (s0 s-inf)
        (append-map-inf/fair g2
          (append-inf/fair (unit s0) s-inf))))))

;(define (once g)
;  (lambda (s)
;    (let loop ((s-inf (g s)))
;      (cond
;        ((null? s-inf) '())
;        ((pair? s-inf)
;         (cons (car s-inf) '()))
;        (else (lambda ()
;                (loop (s-inf))))))))

(define (once g)
  (lambda (s)
    (elim (g s)
      (lambda () (none))
      (lambda (s0 s-inf) (unit s0)))))

;;; Here are the key parts of Appendix A

(define-syntax disj
  (syntax-rules ()
    ((disj) (fail))
    ((disj g0 g ...) (disj+ g0 g ...))))

(define-syntax disj+
  (syntax-rules ()
    ((disj+ g) g)
    ((disj+ g0 g1 g ...) (disj2 g0 (disj+ g1 g ...)))))

(define-syntax conj
  (syntax-rules ()
    ((conj) (fail))
    ((conj g0 g ...) (conj+ g0 g ...))))

(define-syntax conj+
  (syntax-rules ()
    ((conj+ g) g)
    ((conj+ g0 g1 g ...) (conj2 g0 (conj+ g1 g ...)))))

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
