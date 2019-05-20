#lang racket
(provide (all-defined-out))
#| mk-monads |#

#| BEGIN Monad Components |#

;; unit and bind form a Monad.
;; unit, bind, mzero, mplus form a MonadPlus.

(define (unit s) `(,s))

;; Like append-map-inf, but takes arguments in reversed
;; order.
(define (bind/i m1 g1)
  (cond
    ((null? m1) '())
    ((pair? m1)
     (interleave (g1 (car m1))
       (bind/i (cdr m1) g1)))
    (else (lambda ()
            (bind/i (m1) g1)))))

;; Like append-map-inf/fair, but takes arguments in reversed
;; order.
(define (bind m1 g1)
  (cond
    ((null? m1) '())
    ((pair? m1)
     (mplus (g1 (car m1))
              (bind (cdr m1) g1)))
    (else (lambda ()
            (bind (m1) g1)))))

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
    [(null? ms) (mzero)]
    [else (let rec ([ms ms])
            (cond
              [(null? (cdr ms)) (car ms)]
              [else (split ms
                      (lambda (ms1 ms2)
                        (interleave (rec ms1)
                          (rec ms2))))]))]))

#| (→ Space) → Space |#
(define (step f) f)

#| Space A × (→ Space) × (A × Space → Space) → Space |#
;; This function is used in ifte and once.
;; It is like which-List, but perserve cost information
(define (elim m1 kf ks)
  (cond
    [(null? m1) (kf)]
    [(pair? m1) (ks (car m1) (cdr m1))]
    [else (lambda ()
            (elim (m1) kf ks))]))

#| Space A × (→ B) × (A × Space → B) → B |#
;; This function is used in take.
;; It is like which-List.
(define (which m1 kf ks)
  (cond
    [(null? m1) (kf)]
    [(pair? m1) (ks (car m1) (cdr m1))]
    [else (which (m1) kf ks)]))

(define (split ls k)
  (cond
    [(null? ls) (k '() '())]
    [else (split (cdr ls)
            (λ (l1 l2)
              (k (cons (car ls) l2) l1)))]))

#| END Monad Components |#
    
(define DFSi
  (vector mzero unit msum/i bind/i step elim which))
(define DFSbi
  (vector mzero unit msum/b bind/i step elim which))
(define DFSf
  (vector mzero unit msum/f bind/i step elim which))
(define BFS
  (vector mzero unit msum/f bind step elim which))



#| BEGIN Reader Monad |#

(define (return a)
  (lambda (v)
    a))

(define (bind-reader ma sequel)
  (lambda (v)
    (let ([a (ma v)])
      (let ([mb (sequel a)])
        (mb v)))))

(define (mrun ma v) (ma v))

(define (ask)
  (lambda (v) v))

(define-syntax letM
  (syntax-rules ()
    [(letM () body) body]
    [(letM ([x ma] pr ...) body)
     (bind-reader ma (lambda (x) (letM (pr ...) body)))]))

#| END Reader Monad |#



(define BFS-opt
  (let ()
    (define (mzero)  `(()   . #f))
    (define (unit s) `((,s) . #f))
    (define (step f) `(()   . ,f))
    (define (mplus m1 m2)
      (cons (append (car m1) (car m2))
        (let ([t1 (cdr m1)]
              [t2 (cdr m2)])
          (cond
            [(not t1) t2]
            [(not t2) t1]
            [else (lambda () (mplus (t1) (t2)))]))))
    (define (bind m1 g1)
      (foldr
       (lambda (s t-inf)
         (mplus (g1 s) t-inf))
       (let ([f (cdr m1)])
         (step (and f (lambda () (bind (f) g1)))))
       (car m1)))
    (define (msum ms)
      (foldr mplus (mzero) ms))
    (define (elim m1 kf ks)
      (let ([ss (car m1)]
            [f (cdr m1)])
        (cond
          [(and (null? ss) f)
           (step (lambda () (elim (f) kf ks)))]
          [(null? ss) (kf)]
          [else (ks (car ss) (cons (cdr ss) f))])))
    (define (which m1 kf ks)
      (let ([ss (car m1)]
            [f (cdr m1)])
        (cond
          [(and (null? ss) f) (which (f) kf ks)]
          [(null? ss) (kf)]
          [else (ks (car ss) (cons (cdr ss) f))])))
    (vector mzero unit msum bind step elim which)))

;; ask monad components from the reader
(define (ask-field n)
  (letM ([SS (ask)])
    (return (vector-ref SS n))))
(define (ask-mzero) (ask-field 0))
(define (ask-unit)  (ask-field 1))
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
  (letM ([unit (ask-unit)]
         [mzero (ask-mzero)])
    (return (lambda (s)
              (let ((s (unify u v s)))
                (if s (unit s) (mzero)))))))

(define succeed
  (letM ([unit (ask-unit)])
    (return (lambda (s)
              (unit s)))))
 
(define fail
  (letM ([mzero (ask-mzero)])
    (return (lambda (s)
              (mzero)))))

(define (take-inf n s-inf)
  (letM ([which (ask-which)])
    (let T ([n n]
            [s-inf s-inf])
      (cond
        ((and n (zero? n)) (return '()))
        (else
         (which s-inf
           (lambda () (return '()))
           (lambda (s t-inf)
             (letM ([d (T (and n (sub1 n)) t-inf)])
               (return (cons s d))))))))))

(define (conj2 g1 g2)
  (letM ([g1 g1]
         [g2 g2]
         [bind (ask-bind)])
    (return (lambda (s)
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
         [unit (ask-unit)]
         [elim (ask-elim)]
         [bind (ask-bind)]
         [msum (ask-msum)])
    (return
      (lambda (s)
        (elim (g1 s)
          (lambda () (g3 s))
          (lambda (s0 s-inf)
            (bind (msum `(,(unit s0) ,s-inf)) g2)))))))

(define (once g)
  (letM ([g g]
         [mzero (ask-mzero)]
         [unit (ask-unit)]
         [elim (ask-elim)])
    (return
     (lambda (s)
       (elim (g s)
         (lambda () (mzero))
         (lambda (s0 s-inf) (unit s0)))))))


;;; Here are the key parts of Appendix A

(define (disj . gs)
  (letM ([SS (ask)]
         [msum (ask-msum)])
    (let ([gs (map (lambda (g) (mrun g SS)) gs)])
      (return (lambda (s)
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
       (letM ([SS (ask)]
              [step (ask-step)])
         (return (lambda (s)
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
              (letM ([g0 (conj g ...)])
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