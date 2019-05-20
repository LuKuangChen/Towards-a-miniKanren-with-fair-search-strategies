#lang racket/base
(provide (all-defined-out))
#| mk-monads |#

#| BEGIN Monad Components |#

;; unit and bind form a Monad.
;; unit, bind, mzero, and mplus form a MonadPlus.

(define (return s) `(,s))

;; Like append-map-inf, but takes arguments in reversed
;; order.
(define (bind/i m g)
  (cond
    ((null? m) '())
    ((pair? m)
     (interleave (g (car m))
       (bind/i (cdr m) g)))
    (else (lambda ()
            (bind/i (m) g)))))

;; Like append-map-inf/fair, but takes arguments in reversed
;; order.
(define (bind m g)
  (cond
    ((null? m) '())
    ((pair? m)
     (mplus (g (car m))
              (bind (cdr m) g)))
    (else (lambda ()
            (bind (m) g)))))

(define (mzero) '())

;; renamed append-inf 
(define (interleave m1 m2)
  (cond
    ((null? m1) m2)
    ((pair? m1)
     (cons (car m1)
       (interleave (cdr m1) m2)))
    (else (lambda ()
            (interleave m2 (m1))))))

;; renamed append-inf/fair
(define (mplus m1 m2)
  (cond
    ((null? m1) m2)
    ((null? m2) m1)
    ((pair? m1)
     (cons (car m1)
       (mplus (cdr m1) m2)))
    ((pair? m2)
     (cons (car m2)
       (mplus m1 (cdr m2))))
    (else (lambda () (mplus (m1) (m2))))))

;; multi-arity mplus
(define (msum/f ms)
  (foldr mplus (mzero) ms))

;; multi-arity interleave
(define (msum/i ms)
  (foldr interleave (mzero) ms))

;; balanced interleaving
(define (msum/b ms)
  (cond
    ((null? ms) (mzero))
    (else (let rec ((ms ms))
            (cond
              ((null? (cdr ms)) (car ms))
              (else (split ms
                      (lambda (ms1 ms2)
                        (interleave (rec ms1)
                          (rec ms2))))))))))

#| (→ Space) → Space |#
(define (step f) f)

;; The following function is employed by ifte and once.
#| Space × (→ Space) × (State × Space → Space) → Space |#
(define (elim m kf ks)
  (cond
    ((null? m) (kf))
    ((pair? m) (ks (car m) (cdr m)))
    (else (lambda ()
            (elim (m) kf ks)))))

;; The following function is employed by take-inf.
;; It is like which-List.
#| Space × (→ A) × (State × Space → A) → A |#
(define (which m kf ks)
  (cond
    ((null? m) (kf))
    ((pair? m) (ks (car m) (cdr m)))
    (else (which (m) kf ks))))

(define (split ls k)
  (cond
    ((null? ls) (k '() '()))
    (else (split (cdr ls)
            (λ (l1 l2)
              (k (cons (car ls) l2) l1))))))

#| END Monad Components |#
    
(define DFSi
  (vector mzero return msum/i bind/i step elim which))
(define DFSbi
  (vector mzero return msum/b bind/i step elim which))
(define DFSf
  (vector mzero return msum/f bind/i step elim which))
(define BFS
  (vector mzero return msum/f bind step elim which))



#| BEGIN Reader Monad |#

(define (unit a)
  (lambda (v)
    a))

(define (bind-reader ma sequel)
  (lambda (v)
    (let ((a (ma v)))
      (let ((mb (sequel a)))
        (mb v)))))

(define (mrun ma v) (ma v))

(define (ask)
  (lambda (v) v))

(define-syntax letM
  (syntax-rules ()
    ((letM () body) body)
    ((letM ((x ma) pr ...) body)
     (bind-reader ma (lambda (x) (letM (pr ...) body))))))

#| END Reader Monad |#

;; ask monad components from the reader
(define (ask-field n)
  (letM ((SS (ask)))
    (unit (vector-ref SS n))))
(define (ask-mzero) (ask-field 0))
(define (ask-return) (ask-field 1))
(define (ask-msum)  (ask-field 2))
(define (ask-bind)  (ask-field 3))
(define (ask-step)  (ask-field 4))
(define (ask-elim)  (ask-field 5))
(define (ask-which) (ask-field 6))







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
  (letM ((ret (ask-return))
         (mzero (ask-mzero)))
    (unit (lambda (s)
            (let ((s (unify u v s)))
              (if s (ret s) (mzero)))))))

(define succeed
  (letM ((ret (ask-return)))
    (unit (lambda (s)
            (ret s)))))
 
(define fail
  (letM ((mzero (ask-mzero)))
    (unit (lambda (s)
            (mzero)))))

(define (take-inf n s-inf)
  (letM ((which (ask-which)))
    (let T ((n n)
            (s-inf s-inf))
      (cond
        ((and n (zero? n)) (unit '()))
        (else
         (which s-inf
           (lambda () (unit '()))
           (lambda (s t-inf)
             (letM ((d (T (and n (sub1 n)) t-inf)))
               (unit (cons s d))))))))))

(define (conj2 g1 g2)
  (letM ((g1 g1)
         (g2 g2)
         (bind (ask-bind)))
    (unit (lambda (s)
            (bind (g1 s) g2)))))

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
     (unit (lambda (s)
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
  (letM ((g1 g1)
         (g2 g2)
         (g3 g3)
         (ret (ask-return))
         (elim (ask-elim))
         (bind (ask-bind))
         (msum (ask-msum)))
    (unit
      (lambda (s)
        (elim (g1 s)
          (lambda () (g3 s))
          (lambda (s0 s-inf)
            (bind (msum `(,(ret s0) ,s-inf)) g2)))))))

(define (once g)
  (letM ((g g)
         (mzero (ask-mzero))
         (ret (ask-return))
         (elim (ask-elim)))
    (unit
     (lambda (s)
       (elim (g s)
         (lambda () (mzero))
         (lambda (s0 s-inf) (ret s0)))))))


;;; Here are the key parts of Appendix A

(define (disj . gs)
  (letM ((SS (ask))
         (msum (ask-msum)))
    (let ((gs (map (lambda (g) (mrun g SS)) gs)))
      (unit (lambda (s)
              (msum (map (lambda (g) (g s)) gs)))))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (letM ((SS (ask))
              (step (ask-step)))
         (unit (lambda (s)
                 (step
                  (lambda ()
                    ((mrun (conj g ...) SS) s))))))))))

(define-syntax run
  (syntax-rules ()
    ((run SS n (x0 x ...) g ...)
     (run SS n q (fresh (x0 x ...)
                   (== `(,x0 ,x ...) q) g ...)))
    ((run SS n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
            (mrun
              (letM ((g0 (conj g ...)))
                (run-goal n g0))
              SS))))))

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