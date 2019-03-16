(define (disj* gs)
  (cond
    [(null? gs) fail]
    [(null? (cdr gs)) (car gs)]
    [else
     (split gs
       (lambda (gs1 gs2)
         (disj2 (disj* gs1)
                (disj* gs2))))]))

(define (split ls k)
  (cond
    [(null? ls) (k '() '())]
    [else (split (cdr ls)
                 (lambda (l1 l2)
                   (k l2 (cons (car ls) l1))))]))