#| [Goal] x ([Goal] x [Goal] -> Goal) -> Goal |#
(define (split ls k)
  (cond
    [(null? ls) (k '() '())]
    [else (split (cdr ls)
            (lambda (l1 l2)
              (k (cons (car ls) l2) l1)))]))

#| [Goal] -> Goal |#
(define (disj* gs)
  (cond
    [(null? (cdr gs)) (car gs)]
    [else
     (split gs
       (lambda (gs1 gs2)
         (disj2 (disj* gs1)
                (disj* gs2))))]))

(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj g ...) (disj* (list g ...))]))