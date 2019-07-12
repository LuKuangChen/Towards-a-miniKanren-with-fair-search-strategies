#lang slideshow
(require slideshow/code)


(slide
 #:title "Towards a miniKanren with fair search strategies"
 (t "Kuang-Chen Lu")
 (t "Weixi Ma")
 (t "Daniel P. Friedman"))


(slide
 #:title "Where to start?"
 (scale (bitmap "./TRS2.jpg") 0.7))


(define-syntax-rule (repl> c)
  (item #:bullet (text ">" null 30) (code c)))
(define-syntax-rule (repl: c)
  (item #:bullet (blank) (code c)))
(define-syntax-rule (group x ...)
  (hc-append x ...))

(slide
 #:title "What is fairness?"
 (item "fairness in disjunctions (fair, almost-fair, unfair)")
 (item "fairness in conjunctions (fair, unfair)"))


(slide
 #:title "Examples"
 (para (code (repeatᵒ x xs)) "relates" (code x) "with one or more" (code x) "s.")
 (blank)
 'next
 (repl> (run 3 q
          (repeatᵒ 'λ q)))
 (repl: '((λ) (λ λ) (λ λ λ))))


(slide
 #:title "Fairness in Disjunctions"
 (repl> (run 9 q
          (condᵉ
            [(repeatᵒ '🐜 q)]
            [(repeatᵒ '🐦 q)]
            [(repeatᵒ '🌸 q)])))
 'next
 'alts
 (list (list (para "unfair (current search strategy)")
             (repl: '((🐜) (🐜 🐜) (🐦) 
                      (🐜 🐜 🐜) (🌸)
                      (🐜 🐜 🐜 🐜) (🐦 🐦)
                      (🐜 🐜 🐜 🐜 🐜) (🌸 🌸))))
       (list (para "almost-fair")
             (repl: '((🐦) (🐜)
                      (🐦 🐦) (🌸)
                      (🐦 🐦 🐦) (🐜 🐜)
                      (🐦 🐦 🐦 🐦) (🌸 🌸)
                      (🐦 🐦 🐦 🐦 🐦))))
       (list (para "fair")
             (repl: '((🐜) (🐦) (🌸)
                      (🐜 🐜) (🐦 🐦) (🌸 🌸)
                      (🐜 🐜 🐜) (🐦 🐦 🐦) (🌸 🌸 🌸))))))


(define square
  (case-lambda
    [()
     (filled-rectangle 60 60
                       #:border-width 3
                       #:color "white")]
    [(clr)
     (filled-rectangle 60 60
                       #:border-width 3
                       #:color clr)]))
(define (inc)
  (filled-rectangle 30 60
                    #:border-width 3
                    #:color "red"))
(define (stop)
  (filled-rectangle 60 60
                    #:border-width 3))
(slide
 #:title "(Search) Space"
 (tt "Space ::= Null | (Pair State Space) | (→ Space)")
 'next
 (t "a space with three states")
 (hc-append (square) (inc)
            (square) (square)
            (stop))
 (t "a space with possibly infinite states")
 (hc-append (square) (square) (square) (inc)
            (square) (inc)
            (square) (square) (inc)
            (t " ... ...")))


(define (rot spa)
  (rotate spa (* pi 3/2)))

(slide
 #:title "Fairness in Conjunctions"
 (hc-append (square "green") (square "green") (square "green") (inc)
            (square "yellow") (inc)
            (square "lightblue") (square "lightblue") (inc)
            (t " ... ..."))
 (ht-append (rot (hc-append (square "green") (square "green") (inc)
                            (t " ... ...")))
            (rot (hc-append (stop)))
            (rot (hc-append (square "green") (inc)
                            (t " ... ...")))
            (blank 30 60)
            (rot (hc-append (square "yellow") (square "yellow")
                            (stop)
                            #;(inc) #;(t " ... ...")))
            (blank 30 60)
            (rot (hc-append (inc)
                            (t " ... ...")))
            (rot (hc-append (square "lightblue") (inc)
                            (t " ... ...")))
            (blank 30 60)
            (t " ... ...")))

(define (table rows)
  (foldr (curry hc-append 50) (blank)
         (apply map (curry vc-append 5) rows)))

(slide
 #:title "Search Strategies"
 (table (list (list (bt "strategy") (bt "disj") (bt "conj"))
              (list (t "DFSi")  (t "unfair") (t "unfair"))
              (list (t "DFSbi") (t "almost-fair") (t "unfair"))
              (list (t "DFSf")  (t "fair") (t "unfair"))
              (list (t "BFS")   (t "fair") (t "fair")))))


(slide
 #:title "Why fairness?"
 (item "produce answers in less unexpected order")
 (subitem (code repeatᵒ) "examples")
 (subitem "BFS produces answer in order of cost")
 (item "perform more stably when permuting clauses"))


(slide
 (titlet "Q & A"))


