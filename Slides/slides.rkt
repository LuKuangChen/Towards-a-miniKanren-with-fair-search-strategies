#lang slideshow
(require slideshow/code)
(require slideshow/text)
(require racket/draw)

(define DFSi
  (hc-append (t "DFS") (subscript (it "i"))))
(define DFSbi
  (hc-append (t "DFS") (subscript (it "bi"))))
(define DFSf
  (hc-append (t "DFS") (subscript (it "f"))))
(define BFS
  (hc-append (t "BFS")))
(define BFSimp
  (hc-append (t "BFS") (subscript (it "imp"))))
(define BFSser
  (hc-append (t "BFS") (subscript (it "ser"))))

(define IU-icon (bitmap "./IU.png"))

(let ([old-assembler (current-slide-assembler)])
  (current-slide-assembler
   (lambda (title separation content)
     (define old-result (old-assembler title separation content))
     (define adjusted-iu
       (lb-superimpose (blank client-w client-h) (inset (scale IU-icon 0.13) 0 0 0 0)))
     (ct-superimpose old-result adjusted-iu))))

;; visualizer helpers
(begin
  
  (define (ddd)
    (tt " ···"))

  (define (rot spa)
    (rotate spa (* pi 3/2)))

  (define (square clr)
    (filled-rectangle 60 60
                      #:border-width 3
                      #:color clr))
  (define (inc)
    (filled-rectangle 30 60
                      #:border-width 3
                      #:color "gray"))
  (define (stop)
    (filled-rectangle 30 60
                      #:border-width 3
                      #:color "black")))

(define-syntax-rule (repl> c)
  (item #:bullet (text ">" null 30) (code c)))
(define-syntax-rule (repl: c)
  (item #:bullet (blank) (code c)))
(define-syntax-rule (group x ...)
  (hc-append x ...))

(define conde (code condᵉ))

(define new-arrival
  (let* ([red (send the-color-database find-color "Red")]
         [txt (text "NEW ARRIVAL" (list red))]
         [inn (rounded-rectangle 82 20 #:border-color red #:border-width 1)]
         [otr (rounded-rectangle 90 28 #:border-color red #:border-width 3)])
    (rotate (cc-superimpose txt inn otr) (/ pi 12))))


(define (table rows)
  (foldr (curry hc-append 50) (blank)
         (apply map (lambda (h . content)
                      (vr-append 10 h (apply vr-append 5 content)))
                rows)))


(slide
 #:title "Towards a miniKanren with fair search strategies"
 (t "Kuang-Chen Lu")
 (t "Weixi Ma")
 (t "Daniel P. Friedman"))

(slide
 #:title "Content"
 (item "Background")
 (item "Contributions")
 (item "The Behavior of Strategies")
 (item "The Implementation of Strategies")
 (item "Conclusion"))



(slide
 #:title "Content"
 (item "Background ◁")
 (item "Contributions")
 (item "The Behavior of Strategies")
 (item "The Implementation of Strategies")
 (item "Conclusion"))




(slide
 #:title "The Reasoned Schemer, 2nd Edition"
 (scale (bitmap "./TRS2.jpg") 0.7))

(slide
 #:title "Commandments for miniKanren Programmers"
 (item "Within each sequences of goals, move non-recursive goals before recursive goals")
 (item "If your miniKanren program is slow, fiddle with its" conde "-lines.")
 (para "(interleaving DFS, the strategy of"
       (scale (bitmap "./TRS2.jpg") 0.04)
       " and μKanren, is unfair in disjunction)"))

(slide
 #:title "Cases Where We Might Want Other Strategies"
 (item "teaching new miniKanren programmers")
 (item "writing relational definitions that runs in different running modes"))




(slide
 #:title "Content"
 (item "Background ✓")
 (item "Contributions ◁") ;; <--
 (item "The Behavior of Strategies")
 (item "The Implementation of Strategies")
 (item "Conclusion"))

(slide
 #:title "Fairness"
 (item "fairness in disjunctions: unfair, almost-fair, unfair")
 (item "fairness in conjunctions: unfair, fair"))

(slide
 #:title "Search Strategies"
 (table (list (list (bt "strategy") (bt "disj") (bt "conj"))
              (list (t "interleaving DFS")
                    (t "unfair") (t "unfair"))
              (list (t "balanced interleaving DFS")
                    (t "almost-fair") (t "unfair"))
              (list (t "fair DFS")
                    (t "fair") (t "unfair"))
              (list (t "BFS[1]")
                    (t "fair") (t "fair"))))
 (text "[1] Seres, Silvija, J. Michael Spivey, and C. A. R. Hoare. \"Algebra of Logic Programming.\" ICLP. 1999."
       (current-main-font)
       (round (* (current-font-size) 0.5))))

(define (mark-new x)
  (lt-superimpose x (inset new-arrival -60 -10 +60 -20)))

(slide
 #:title "Search Strategies"
 (table (list (list (bt "strategy") (bt "disj") (bt "conj"))
              (list (t "interleaving DFS")
                    (t "unfair") (t "unfair"))
              (list (mark-new (t "balanced interleaving DFS"))
                    (t "almost-fair") (t "unfair"))
              (list (mark-new (t "fair DFS"))
                    (t "fair") (t "unfair"))
              (list (t "BFS[1]")
                    (t "fair") (t "fair"))))
 (text "[1] Seres, Silvija, J. Michael Spivey, and C. A. R. Hoare. \"Algebra of Logic Programming.\" ICLP. 1999."
       (current-main-font)
       (round (* (current-font-size) 0.5))))





(slide
 #:title "Content"
 (item "Background ✓")
 (item "Contributions ✓")
 (item "The Behavior of Strategies ◁") ;; <-
 (item "The Implementation of Strategies")
 (item "Conclusion"))

(slide
 (code (repeatᵒ x l))
 (item (code x) "is an arbitrary thing.")
 (item (code l) "is a list of one or more" (code x) "s.")
 (blank)
 (repl> (run 3 q
          (repeatᵒ 'λ q)))
 (repl: '((λ) (λ λ) (λ λ λ))))

(slide
 #:title "Interleaving DFS (Unfair Disjunction)"
 (repl> (run 15 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)]
            [(repeatᵒ '🐈 q)]
            [(repeatᵒ '🐕 q)])))
 (repl: '((λ) (λ λ) (🐑) (λ λ λ) (λ λ λ λ)
          (🐑 🐑) (λ λ λ λ λ) (🐏)
          (λ λ λ λ λ λ) (🐑 🐑 🐑)
          (λ λ λ λ λ λ λ) (λ λ λ λ λ λ λ λ)
          (🐑 🐑 🐑 🐑) (λ λ λ λ λ λ λ λ λ)
          (🐏 🐏))))

(slide
 #:title "Balanced-interleaving DFS (Almost-fair Disjunction)"
 (repl> (run 15 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)]
            [(repeatᵒ '🐈 q)]
            [(repeatᵒ '🐕 q)])))
 (repl: '((🐑) (🐏) (🐈) (λ)
          (🐑 🐑) (🐏 🐏) (🐈 🐈) (🐕)
          (🐑 🐑 🐑) (🐏 🐏 🐏) (🐈 🐈 🐈) (λ λ)
          (🐑 🐑 🐑 🐑) (🐏 🐏 🐏 🐏) (🐈 🐈 🐈 🐈))))
(define almost-fair-example (most-recent-slide))

(slide
 #:title "Fair DFS & BFS (Fair Disjunction)"
 (repl> (run 15 q
          (condᵉ
            [(repeatᵒ 'λ q)]
            [(repeatᵒ '🐑 q)]
            [(repeatᵒ '🐏 q)]
            [(repeatᵒ '🐈 q)]
            [(repeatᵒ '🐕 q)])))
 (repl: '((λ) (🐑) (🐏)
          (🐈) (🐕)
          (λ λ) (🐑 🐑) (🐏 🐏)
          (🐈 🐈) (🐕 🐕)
          (λ λ λ) (🐑 🐑 🐑) (🐏 🐏 🐏)
          (🐈 🐈 🐈) (🐕 🐕 🐕))))




(slide
 #:title "Content"
 (item "Background ✓")
 (item "Contributions ✓")
 (item "The Behavior of Strategies ✓")
 (item "The Implementation of Strategies ◁") ;; <-
 (item "Conclusion"))

(slide
 #:title "Review"
 (item "a" (code goal) "is a function from a" (code state) "to a" (code space))
 (item "a" (code state) "is a way to satisfy some relations")
 (item "a (search)" (code space) "is a collection of" (code state) "s"))

(slide
 #:title "(Search) Space"
 (tt "Space ::= Null | (→ Space) | (Pair State Space)")
 'next
 (t "a space with three states")
 (hc-append (square "ivory") (inc)
            (square "ivory") (square "ivory")
            (stop))
 'next
 (t "a space with possibly infinite states")
 (hc-append (square "ivory") (square "ivory") (square "ivory") (inc)
            (square "ivory") (inc)
            (square "ivory") (square "ivory") (inc)
            (ddd)))

(slide
 #:title "Implementation of Unfair Disjunction"
 (hc-append
  20
  (dc (λ (dc dx dy)
        (define old-pen (send dc get-pen))
        (send dc set-pen
              (new pen% [color "black"]))
        (define path (new dc-path%))
        (send path move-to 0 190)
        (send path line-to 180 350)
        (for ([i (in-range 4)])
          (send path move-to (* 180 (/ i 4)) (+ 190 (* (- 350 190) (/ i 4))))
          (send path line-to 180 (+ 30 (* i 80))))
        (send dc draw-path path dx dy)
        (send dc set-pen old-pen))
      180 ;(* 60 3)
      380 ;(- (* 80 5) 20)
      )
  (vl-append
   20
   (let ([block (hc-append (square "gold") (inc))])
     (hc-append block block block block (ddd)))
   (let ([block (hc-append (square "yellow") (inc))])
     (hc-append block block block block (ddd)))
   (let ([block (hc-append (square "white") (inc))])
     (hc-append block block block block (ddd)))
   (let ([block (hc-append (square "Pale Green") (inc))])
     (hc-append block block block block (ddd)))
   (let ([block (hc-append (square "Lime Green") (inc))])
     (hc-append block block block block (ddd))))))

(define unfair-impl 
  (most-recent-slide))

(slide
   #:title "Implementation of Unfair Disjunction"
   (hc-append
    20
    (dc (λ (dc dx dy)
          (define old-pen (send dc get-pen))
          (send dc set-pen
                (new pen% [color "black"]))
          (define path (new dc-path%))
          (send path move-to (* 180 (/ 3 4)) (+ 190 (* (- 350 190) (/ 3 4))))
          (send path line-to 180 (+ 30 (* 3 80)))
          (send path move-to (* 180 (/ 3 4)) (+ 190 (* (- 350 190) (/ 3 4))))
          (send path line-to 180 (+ 30 (* 4 80)))
          (send dc draw-path path dx dy)
          (send dc set-pen old-pen))
        180 ;(* 60 3)
        380 ;(- (* 80 5) 20)
        )
    (vl-append
     20
     (blank 60 60)
     (blank 60 60)
     (blank 60 60)
     (let ([block (hc-append (square "Pale Green") (inc))])
       (hc-append block block block block (ddd)))
     (let ([block (hc-append (square "Lime Green") (inc))])
       (hc-append block block block block (ddd))))))

(define append-inf-greens
  (dc (λ (dc dx dy)
        (define old-pen (send dc get-pen))
        (send dc set-pen
              (new pen% [color "black"]))
        (define path (new dc-path%))
        (define width (* 180 1/4))
        (define height 140)
        (send path move-to 0 (/ height 2))
        (send path line-to width 30)
        (send path move-to 0 (/ height 2))
        (send path line-to width (+ 30 80))
        (send dc draw-path path dx dy)
        (send dc set-pen old-pen))
      (* 180 1/4) ;(* 60 3)
      140 ;(- (* 80 5) 20)
      ))


(slide
 #:title "Implementation of Unfair Disjunction"
 (vl-append
  60
  (hc-append
   20 append-inf-greens
   (vl-append
    20
    (let ([block (hc-append (square "Pale Green") (inc))])
      (hc-append block block block block (ddd)))
    (let ([block (hc-append (square "Lime Green") (inc))])
      (hc-append block block block block (ddd)))))
  ;; result of combination
  (blank 60 60)))

(define hole (blank 90 60))
(slide
 #:title "Implementation of Unfair Disjunction"
 (vl-append
  60
  (hc-append
   20 append-inf-greens
   (vl-append
    20
    (let ([block (hc-append (square "Pale Green") (inc))])
      (hc-append hole block block block (ddd)))
    (let ([block (hc-append (square "Lime Green") (inc))])
      (hc-append block block block block (ddd)))))
  ;; result of combination
  (let ([block1 (hc-append (square "Pale Green") (inc))]
        [block2 (hc-append (square "Lime Green") (inc))])
    (hc-append block1))))

(slide
 #:title "Implementation of Unfair Disjunction"
 (vl-append
  60
  (hc-append
   20 append-inf-greens
   (vl-append
    20
    (let ([block (hc-append (square "Pale Green") (inc))])
      (hc-append hole block block block (ddd)))
    (let ([block (hc-append (square "Lime Green") (inc))])
      (hc-append hole block block block (ddd)))))
  ;; result of combination
  (let ([block1 (hc-append (square "Pale Green") (inc))]
        [block2 (hc-append (square "Lime Green") (inc))])
    (hc-append block1 block2))))

(slide
 #:title "Implementation of Unfair Disjunction"
 (vl-append
  60
  (hc-append
   20 append-inf-greens
   (vl-append
    20
    (let ([block (hc-append (square "Pale Green") (inc))])
      (hc-append hole hole block block (ddd)))
    (let ([block (hc-append (square "Lime Green") (inc))])
      (hc-append hole block block block (ddd)))))
  ;; result of combination
  (let ([block1 (hc-append (square "Pale Green") (inc))]
        [block2 (hc-append (square "Lime Green") (inc))])
    (hc-append block1 block2 block1))))

(slide
 #:title "Implementation of Unfair Disjunction"
 ;; finally ...
 (let ([block1 (hc-append (square "Pale Green") (inc))]
       [block2 (hc-append (square "Lime Green") (inc))])
   (hc-append block1 block2 block1 block2 block1 block2 block1 block2 (ddd))))

(slide
 (code (define (append-inf s-inf t-inf)
         (cond
           ((null? s-inf) t-inf)
           ((pair? s-inf)
            (cons (car s-inf)
              (append-inf (cdr s-inf) t-inf)))
           (else (lambda () 
                   (append-inf t-inf (s-inf))))))))

(slide
 #:title "Implementation of Almost-fair Disjunction"
 (let* ([sp1 (let ([block (hc-append (square "gold") (inc))])
               (hc-append block block block block (ddd)))]
        [sp2 (let ([block (hc-append (square "yellow") (inc))])
               (hc-append block block block block (ddd)))]
        [sp3 (let ([block (hc-append (square "white") (inc))])
               (hc-append block block block block (ddd)))]
        [sp4 (let ([block (hc-append (square "Pale Green") (inc))])
               (hc-append block block block block (ddd)))]
        [sp5 (let ([block (hc-append (square "Lime Green") (inc))])
               (hc-append block block block block (ddd)))])
   (vc-append
    60
    (vl-append 20 sp1 sp2 sp3 sp4 sp5)
    (hc-append
     100
     (scale (vl-append 20 sp1 sp3 sp5) 0.5)
     (scale (vl-append 20 sp2 sp4) 0.6)))))
(re-slide almost-fair-example)

(slide
 #:title "Implementation of Fair Disjunction"
 (let* ([sp1 (let ([block (hc-append (square "gold") (inc))])
               (hc-append block block block block (ddd)))]
        [sp2 (let ([block (hc-append (square "yellow") (inc))])
               (hc-append block block block block (ddd)))]
        [sp3 (let ([block (hc-append (square "white") (inc))])
               (hc-append block block block block (ddd)))]
        [sp4 (let ([block (hc-append (square "Pale Green") (inc))])
               (hc-append block block block block (ddd)))]
        [sp5 (let ([block (hc-append (square "Lime Green") (inc))])
               (hc-append block block block block (ddd)))])
   (vl-append 20 sp1 sp2 sp3 sp4 sp5))
 (let* ([b1 (square "gold")]
        [b2 (square "yellow")]
        [b3 (square "white")]
        [b4 (square "Pale Green")]
        [b5 (square "Lime Green")]
        [block (hc-append b1 b2 b3 b4 b5 (inc))])
   (scale (hc-append block block block block (ddd)) 0.5)))

(slide
 (code (define (append-inf/fair s-inf t-inf)
         (let loop ((s? #t) (s-inf s-inf) (t-inf t-inf))
           (cond
             ((null? s-inf) t-inf)
             ((pair? s-inf)
              (cons (car s-inf)
                (loop s? (cdr s-inf) t-inf)))
             (s? (loop #f t-inf s-inf))
             (else (lambda ()
                     (loop #t (t-inf) (s-inf)))))))))

(slide
 #:title "Implementations of Conjunctions"
 ;; Move g1 and g2 such that
 ;; g1 is over the first box
 ;; and g2 is over the first space to the right
 (ht-append
  (vc-append
   (code g1)
   (blank 20 20)
   (rot (hc-append (square "green") (square "green") (square "green") (inc)
                   (square "yellow") (inc)
                   (square "lightblue") (square "lightblue") (inc)
                   (ddd))))
  (blank 40 60)
  (vc-append
   (code (conj2 g1 g2))
   (blank 20 20)
   (let ([smaller-hc-append
          (lambda ps
            (vl-append
             (blank 10 (* 60 1/10))
             (scale (apply hc-append ps) 4/5)
             (blank 10 (* 60 1/10))))])
     (vl-append (smaller-hc-append (square "green") (square "green") (inc) (ddd))
                (smaller-hc-append (stop))
                (smaller-hc-append (square "green") (inc) (ddd))
                (blank 60 30)
                (smaller-hc-append (square "yellow") (square "yellow") (stop))
                (blank 60 30)
                (smaller-hc-append (inc) (ddd))
                (smaller-hc-append (square "lightblue") (inc) (ddd))
                (blank 60 30)
                (rot (ddd)))))))

(slide
 (code (define (append-map-inf g s-inf)
         (cond
           ((null? s-inf) '())
           ((pair? s-inf)
            (append-inf (g (car s-inf))
              (append-map-inf g (cdr s-inf))))
           (else (lambda () 
                   (append-map-inf g (s-inf))))))))

(slide
 #:title "Content"
 (item "Background ✓")
 (item "Contributions ✓")
 (item "The Behavior of Strategies ✓")
 (item "The Implementation of Strategies ✓")
 (item "Conclusion ◁"))

(slide
 #:title "Quantitative Evaluation"
 (table (list (list (bt "Benchmark")
                    (bold DFSi) (bold DFSbi) (bold DFSf) (bold BFSimp) (bold BFSser))
              (list (t "quine-1")
                    (t "41") (t "48") (t "33") (t "-") (t "-"))
              (list (t "quine-2")
                    (t "72") (t "76") (t "32") (t "-") (t "-"))
              (list (t "'(I love you)-1")
                    (t "78") (t "70") (t "59") (t "254") (t "605"))
              (list (t "'(I love you)-2")
                    (t "631") (t "173") (t "61") (t "253") (t "605")))))

(slide
 #:title "Why Fairness?"
 (item "resistant to permutation of" (code condᵉ) "lines.")
 (subitem "need not be concerned about line order.")
 (subitem "one definition for different running modes")
 (item "more understandable order of answers"))


(slide
 (titlet "Q & A"))

 
