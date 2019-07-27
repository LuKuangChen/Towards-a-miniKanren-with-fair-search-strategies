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

#;
(slide
 #:title "Fairness in Disjunctions"
 (repl> (run 9 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)])))
 'next
 'alts
 (list (list (para "unfair (current search strategy)")
             (repl: '((λ) (λ λ) (🐑) 
                      (λ λ λ) (🐏)
                      (λ λ λ λ) (🐑 🐑)
                      (λ λ λ λ λ) (🐏 🐏))))
       (list (para "almost-fair")
             (repl: '((🐑) (λ)
                      (🐑 🐑) (🐏)
                      (🐑 🐑 🐑) (λ λ)
                      (🐑 🐑 🐑 🐑) (🐏 🐏)
                      (🐑 🐑 🐑 🐑 🐑))))
       (list (para "fair")
             (repl: '((λ) (🐑) (🐏)
                      (λ λ) (🐑 🐑) (🐏 🐏)
                      (λ λ λ) (🐑 🐑 🐑) (🐏 🐏 🐏))))))


(slide
 #:title "Fairness in Disjunctions"
 (repl> (run 9 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)])))
 (para "unfair (current search strategy)")
 (repl: '((λ) (λ λ) (🐑) 
          (λ λ λ) (🐏)
          (λ λ λ λ) (🐑 🐑)
          (λ λ λ λ λ) (🐏 🐏))))


(slide
 #:title "Fairness in Disjunctions"
 (repl> (run 9 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)])))
 (para "almost-fair")
 (repl: '((🐑) (λ)
          (🐑 🐑) (🐏)
          (🐑 🐑 🐑) (λ λ)
          (🐑 🐑 🐑 🐑) (🐏 🐏)
          (🐑 🐑 🐑 🐑 🐑))))


(slide
 #:title "Compare fair and almost-fair"
 (repl> (run 15 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)]
            [(repeatᵒ '🐄 q)]
            [(repeatᵒ '🐎 q)])))
 (para "unfair")
 (repl: '((λ) (λ λ) (🐑) (λ λ λ) (λ λ λ λ)
          (🐑 🐑) (λ λ λ λ λ) (🐏) (λ λ λ λ λ λ)
          (🐑 🐑 🐑) (λ λ λ λ λ λ λ) (λ λ λ λ λ λ λ λ)
          (🐑 🐑 🐑 🐑) (λ λ λ λ λ λ λ λ λ) (🐏 🐏))))

(slide
 #:title "Compare fair and almost-fair"
 (repl> (run 15 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)]
            [(repeatᵒ '🐄 q)]
            [(repeatᵒ '🐎 q)])))
 (para "almost-fair")
 (repl: '((🐑) (🐏) (🐄) (λ)
          (🐑 🐑) (🐏 🐏) (🐄 🐄) (🐎)
          (🐑 🐑 🐑) (🐏 🐏 🐏) (🐄 🐄 🐄) (λ λ)
          (🐑 🐑 🐑 🐑) (🐏 🐏 🐏 🐏) (🐄 🐄 🐄 🐄))))

(slide
 #:title "Fairness in Disjunctions"
 (repl> (run 9 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)])))
 (para "fair")
 (repl: '((λ) (🐑) (🐏)
          (λ λ) (🐑 🐑) (🐏 🐏)
          (λ λ λ) (🐑 🐑 🐑) (🐏 🐏 🐏))))


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
                    #:color "gray"))
(define (stop)
  (filled-rectangle 30 60
                    #:border-width 3
                    #:color "black"))

(define (ddd)
  (tt "···"))

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
            (ddd)))


(define (rot spa)
  (rotate spa (* pi 3/2)))


(slide
 #:title "Fairness in Conjunctions"
 'next
 (ht-append
  (rot (hc-append (square "green") (square "green") (square "green") (inc)
                  (square "yellow") (inc)
                  (square "lightblue") (square "lightblue") (inc)
                  (ddd)))
  (blank 60 60)
  (vl-append (hc-append (square "green") (square "green") (inc)
                        (ddd))
             (hc-append (stop))
             (hc-append (square "green") (inc)
                        (ddd))
             (blank 60 30)
             (hc-append (square "yellow") (square "yellow")
                        (stop))
             (blank 60 30)
             (hc-append (inc)
                             (ddd))
             (hc-append (square "lightblue") (inc)
                             (ddd))
             (blank 60 30)
             (rot (ddd)))))

#;
(slide
 #:title "Fairness in Conjunctions"
 'next
 (rot (hc-append (square "green") (square "green") (square "green") (inc)
                 (square "yellow") (inc)
                 (square "lightblue") (square "lightblue") (inc)
                 (ddd)))
 (ht-append (rot (hc-append (square "green") (square "green") (inc)
                            (ddd)))
            (rot (hc-append (stop)))
            (rot (hc-append (square "green") (inc)
                            (ddd)))
            (blank 30 60)
            (rot (hc-append (square "yellow") (square "yellow")
                            (stop)
                            #;(inc) #;(ddd)))
            (blank 30 60)
            (rot (hc-append (inc)
                            (ddd)))
            (rot (hc-append (square "lightblue") (inc)
                            (ddd)))
            (blank 30 60)
            (ddd)))

(define (table rows)
  (foldr (curry hc-append 50) (blank)
         (apply map (lambda (h . content)
                      (vr-append 10 h (apply vr-append 5 content)))
                rows)))

(slide
 #:title "Search Strategies"
 (table (list (list (bt "strategy") (bt "disj") (bt "conj"))
              (list (t "interleaving DFS")  (t "unfair") (t "unfair"))
              (list (t "balanced interleaving DFS") (t "almost-fair") (t "unfair"))
              (list (t "fair DFS")  (t "fair") (t "unfair"))
              (list (t "BFS[1]")   (t "fair") (t "fair"))))
 (text "[1] Seres, Silvija, J. Michael Spivey, and C. A. R. Hoare. \"Algebra of Logic Programming.\" ICLP. 1999."
       (current-main-font)
       (round (* (current-font-size) 0.5))))


(slide
 #:title "Why fairness?"
 (item "produce answers in a more natural order")
 (item "performance is resistant to permuting" (code condᵉ) "clauses")
 (subitem "less pitfalls for beginners")
 (subitem "one definition for many running modes"))


(slide
 (titlet "Q & A"))

 
