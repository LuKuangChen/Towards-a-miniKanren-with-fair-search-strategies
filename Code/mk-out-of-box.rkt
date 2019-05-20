#lang racket
(provide (all-defined-out))
#| mk-out-of-box

customizable search strategy

 |#

;; Reader monad

(define (return a)
  (lambda (v)
    a))

(define (bind ma sequel)
  (lambda (v)
    (let ([a (ma v)])
      (let ([mb (sequel a)])
        (mb v)))))

(define (mrun ma v) (ma v))

(define (ask n)
  (lambda (v) (vector-ref v n)))
(define (ask-all)
  (lambda (v) v))

(define-syntax letM
  (syntax-rules ()
    [(letM () body) body]
    [(letM ([x ma] pr ...) body)
     (bind ma (lambda (x) (letM (pr ...) body)))]))



(define (none)   '())
(define (unit s) `(,s))

;; auxiliary search strategies

(define (append-inf/i s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf)
       (append-inf/i (cdr s-inf) t-inf)))
    (else (lambda ()
            (append-inf/i t-inf (s-inf))))))

;; s-infs is not empty
(define (append-inf*/i s-infs)
  (foldr append-inf/i (none) s-infs))



(define (split ls k)
  (cond
    [(null? ls) (k '() '())]
    [else
     (split (cdr ls)
       (λ (l1 l2)
         (k (cons (car ls) l2) l1)))]))

;; s-infs is not empty
(define (append-inf*/bi s-infs)
  (cond
    [(null? (cdr s-infs)) (car s-infs)]
    [else
     (split s-infs
            (lambda (xs ys)
              (append-inf/i (append-inf*/bi xs)
                            (append-inf*/bi ys))))]))



(define (append-inf/f s-inf t-inf)
  (let loop ([s? #t]
             [s-inf s-inf]
             [t-inf t-inf])
    (cond
      ((pair? s-inf)
       (cons (car s-inf)
         (loop s? (cdr s-inf) t-inf)))
      ((null? s-inf) t-inf)
      (s? (loop #f t-inf s-inf))
      (else (lambda () (loop #t (t-inf) (s-inf)))))))

;; s-infs is not empty
(define (append-inf*/f s-infs)
  (foldr append-inf/f (none) s-infs))





(define (append-map-inf/i g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf/i (g (car s-inf))
       (append-map-inf/i g (cdr s-inf))))
    (else (lambda ()
            (append-map-inf/i g (s-inf))))))



(define (append-map-inf/f g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf/f (g (car s-inf))
       (append-map-inf/f g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf/f g (s-inf))))))





(define DFSi  (vector append-inf*/i append-map-inf/i))
(define DFSbi (vector append-inf*/i append-map-inf/i))
(define DFSf  (vector append-inf*/f append-map-inf/i))
(define BFS   (vector append-inf*/f append-map-inf/f))




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
  (return (lambda (s)
            (let ((s (unify u v s)))
              (if s `(,s) '())))))

(define succeed
  (return (lambda (s)
            `(,s))))
 
(define fail
  (return (lambda (s)
            '())))

(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null? s-inf) '())
    ((pair? s-inf) 
     (cons (car s-inf)
       (take-inf (and n (sub1 n))
         (cdr s-inf))))
    (else (take-inf n (s-inf)))))

(define (conj2 g1 g2)
  (letM ([g1 g1]
         [g2 g2]
         [append-map-inf (ask 1)])
    (return (lambda (s)
              (append-map-inf g2 (g1 s))))))

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
     (return (lambda (s)
               (let ((x (walk* x s)) ...)
                 ((conj g ...) s)))))))

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

(define (ifte g1 g2 g3)
  (letM ([g1 g1]
         [g2 g2]
         [g3 g3]
         [append-map-inf (ask 1)])
    (return (lambda (s)
              (let loop ((s-inf (g1 s)))
                (cond
                  ((null? s-inf) (g3 s))
                  ((pair? s-inf)
                   (append-map-inf g2 s-inf))
                  (else (lambda ()
                          (loop (s-inf))))))))))

(define (once g)
  (letM ([g g])
    (return (lambda (s)
              (let loop ((s-inf (g s)))
                (cond
                  ((null? s-inf) '())
                  ((pair? s-inf)
                   (cons (car s-inf) '()))
                  (else (lambda ()
                          (loop (s-inf))))))))))


;;; Here are the key parts of Appendix A

(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g0 g ...) (disj* g0 g ...))))

(define (disj* . gs)
  (letM ([append-inf* (ask 0)]
         [SS (ask-all)])
    (let ([gs (map (lambda (g) (mrun g SS)) gs)])
      (return
        (lambda (s)
          (append-inf* (map (lambda (g) (g s)) gs)))))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (letM ([SS (ask-all)])
         (return (lambda (s)
                   (lambda ()
                     ((mrun (conj g ...) SS) s)))))))))

(define-syntax run
  (syntax-rules ()
    ((run SS n (x0 x ...) g ...)
     (run SS n q (fresh (x0 x ...)
                   (== `(,x0 ,x ...) q) g ...)))
    ((run SS n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
         (run-goal n (mrun (conj g ...) SS)))))))

(define-syntax run*
  (syntax-rules ()
    ((run* SS q g ...) (run SS #f q g ...))))

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