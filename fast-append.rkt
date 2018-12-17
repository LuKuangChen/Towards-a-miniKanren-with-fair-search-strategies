#lang racket
#| fast-append |#

(provide (rename-out (make-++ @++))
         @car×cdr
         @list
         @null
         @null?
         ;; profile
         @length)

;; used for profiling
(define (@length @)
  (match @
    [(@null) 0]
    [else (let loop ([@ @])
            (match @
              [(@list x) 1]
              [(@++ l r) (+ (loop l) (loop r))]))]))

#| IMPLEMENTATION |#

(struct @null () #:transparent)
(struct @list (item) #:transparent)

; Users call 'make-++ instead of '@++.
(struct @++ (left right) #:transparent)

#| List × List → List |#
(define (make-++ left right)
  (cond
    [(@null? left) right]
    [(@null? right) left]
    [else (@++ left right)]))

#| List → X × List |#
(define (@car×cdr cur)
  (match cur
    [(@null) (error "no element remains")]
    [(@list item) (values item (@null))]
    [(@++ left right)
     (let loop ([cur left]
                [acc right])
       (match cur
         [(@++ l1 r1) (loop l1 (@++ r1 acc))]
         [(@list item) (values item acc)]))]))
