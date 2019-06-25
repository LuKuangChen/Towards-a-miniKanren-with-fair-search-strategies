#lang racket
(require "../mk-3-1-ca-1.rkt")
#|

part of intuinitionistic Zeroth-Order Logic

only →   →
and  &   Pair
and  |   Union

|#


(defrel (lookupo Γ x v)
  (let ([q (unbox qbox)])
    (project (q Γ)
      (begin
        ;(displayln `(lookup ,Γ ,q))
        (fresh (y u Γ^)
          (== `((,y . ,u) . ,Γ^) Γ)
          (conde
            [(== y x) (== u v)]
            [(lookupo Γ^ x v)]))))))

(define counter (box 0))

(defrel (⊢ his Γ e τ)
  (let ([q (unbox qbox)])
    (project (q his Γ e τ)
      (begin
        (set-box! counter (add1 (unbox counter)))
        (displayln (list (unbox counter) #;his))
        (conde
          [(fresh (name)
             (== `(the (,name)) e)
             (== `(,name) τ))]
          [(fresh (y)
             (== `(var ,y) e)
             (lookupo Γ y τ))]
          [(fresh (x body τ1 τ2)
             (== `(λ (,x : ,τ1) ,body) e)
             (== `(→ ,τ1 ,τ2) τ)
             (⊢ (cons 'λ his) `((,x . ,τ1) . ,Γ) body τ2))]
          [(fresh (a d τ1 τ2)
             (== `(cons ,a ,d) e)
             (== `(Π ,τ1 ,τ2) τ)
             (⊢ (cons 'cons his) Γ a τ1)
             (⊢ (cons 'cons his) Γ d τ2))]
          [(fresh (e1 τ1 τ2)
             (== `(inl ,e1 ,τ2) e)
             (== `(Σ ,τ1 ,τ2) τ)
             (⊢ (cons 'uL his) Γ e1 τ1))]
          [(fresh (e1 τ1 τ2)
             (== `(inr ,τ1 ,e1) e)
             (== `(Σ ,τ1 ,τ2) τ)
             (⊢ (cons 'uR his) Γ e1 τ2))]
          [(fresh (rator rand τ1)
             (== `(app ,rator ,rand) e)
             (⊢ (cons 'app his) Γ rator `(→ ,τ1 ,τ))
             (⊢ (cons 'app his) Γ rand τ1))]
          [(fresh (pr τ1)
             (== `(car ,pr) e)
             (⊢ (cons 'car his) Γ pr `(Π ,τ ,τ1)))]
          [(fresh (pr τ1)
             (== `(cdr ,pr) e)
             (⊢ (cons 'cdr his) Γ pr `(Π ,τ1 ,τ)))]
          [(fresh (ue x1 b1 x2 b2 τ1 τ2)
             (== `(case ,ue [,x1 => ,b1] [,x2 => ,b2]) e)
             (⊢ (cons 'case his) `((,x1 . ,τ1) . ,Γ) b1 τ)
             (⊢ (cons 'case his) `((,x2 . ,τ2) . ,Γ) b2 τ)
             (⊢ (cons 'case his) Γ ue `(Σ ,τ1 ,τ2)))])))))

(define qbox (box #f))

;; mk-0    : 9
;; mk-3    : 10
;; mk-3-1  : 10
#;
(run 1 q
  (begin
    (set-box! qbox q)
    (⊢ '() '((x . (Π A B)))
       q
       'A)))


;; mk-fair : 11459
;; mk-0    : 140
;; mk-3    : 25036
;; mk-3-1  : 26351
;; mk-lazy :
#;
(run 1 q
  (begin
    (set-box! qbox q)
    (⊢ '() '((x . (Π A B)))
       q
       '(→ (Π A B) (Π B A)))))

;; mk-fair : 12427
;; mk-0    : 
;; mk-3    : 
;; mk-4    :
(run 1 q
  (begin
    (set-box! qbox q)
    (⊢ '() '() q
       '(→ (Σ A B) (Σ B A)))))