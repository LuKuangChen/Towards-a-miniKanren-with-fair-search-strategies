#lang racket
#| test cases from Chapter 6 of TRS2|#
(require "../mk-monads.rkt")

(define SS DFSf)

; frame 1
(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(run SS 1 q
  (alwayso))
;'(_0)

(run SS 1 q
  (conde
    [succeed]
    [(alwayso)]))
;'(_0)

#|
(run* q
  (alwayso))
; no value
|#

#|
(run* q
  (conde
    [succeed]
    [(alwayso)]))
; no value
|#

(run SS 5 q
  (alwayso))
;'(_0 _0 _0 _0 _0)

(run SS 5 q
  (== 'onion q)
  (alwayso))
;'(onion onion onion onion onion)

#|
(run BFS 1 q
  (alwayso)
  fail)
; no value
|#

(run SS 1 q
  (== 'garlic q)
  succeed
  (== 'onion q))
;'()

#|
(run BFS 1 q
  (== 'garlic q)
  (alwayso)
  (== 'onion q))
; no value
|#

(run SS 1 q
  (conde
    [(== 'garlic q) (alwayso)]
    [(== 'onion q)])
  (== 'onion q))
;'(onion)

#|
(run BFS 2 q
  (conde
    [(== 'garlic q) (alwayso)]
    [(== 'onion q)])
  (== 'onion q))
; no value
|#

(run SS 5 q
  (conde
    [(== 'garlic q) (alwayso)]
    [(== 'onion q) (alwayso)])
  (== 'onion q))
;'(onion onion onion onion onion)

; frame 14
(defrel (nevero)
  (nevero))

#|
(run BFS 1 q
  (nevero))
; no value
|#

(run SS 1 q
  fail
  (nevero))
;'()

(run SS 1 q
  (conde
    [succeed]
    [(nevero)]))
;'(_0)

(run SS 1 q
  (conde
    [(nevero)]
    [succeed]))
;'(_0)

#|
(run BFS 2 q
  (conde
    [succeed]
    [(nevero)]))
; no value
|#

#|
(run BFS 1 q
  (conde
    [succeed]
    [(nevero)])
  fail)
; no value
|#

(run SS 5 q
  (conde
    [(nevero)]
    [(alwayso)]
    [(nevero)]))
;'(_0 _0 _0 _0 _0)

(run SS 6 q
  (conde
    [(== 'spicy q) (nevero)]
    [(== 'hot q) (nevero)]
    [(== 'apple q) (alwayso)]
    [(== 'cider q) (alwayso)]))
;'(apple cider apple cider apple cider)

; frame 24
(defrel (very-recursiveo)
  (conde
    [(nevero)]
    [(very-recursiveo)]
    [(alwayso)]
    [(very-recursiveo)]
    [(nevero)]))

;; the number is 1000000 in TRS2
(length (run SS 100 q (very-recursiveo)))