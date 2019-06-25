#lang racket
#| test cases from Chapter 6 of TRS2|#
(require "../mk-3-1-ca.rkt")
;(require "../mk-3.rkt")
;(require "../mk-3-alternative-append-map.rkt")
;(require "../mk-fair.rkt")

(defrel (nevero)
  (nevero))


(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

; frame 1
(defrel (rec2o)
  (begin
    ;(set-box! counter (add1 (unbox counter)))
    ;(displayln (unbox counter))
    (conde
      [(nevero)]
      [(rec2o)]
      [(alwayso)]
      [(rec2o)]
      [(nevero)])))

(define counter (box 0))

;; the number is 1000000 in TRS2
(length (run 1000000 q (rec2o)))