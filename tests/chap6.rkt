#lang racket
#| chap6 |#

(require "../mk.rkt")

; FRAME 1
(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(run 1 q
  (alwayso))
;; '(_0)

(run 1 q
  (conde
    [succeed]
    [(alwayso)]))
;; '(_0)

#|
(run* q
  (alwayso))
;; no value
|#

#|
(run* q
  (conde
    [succeed]
    [(alwayso)]))
;; no value
|#

(run 5 q
  (alwayso))
;; '(_0 _0 _0 _0 _0)

(run 5 q
  (== 'onion q)
  (alwayso))
;; '(onion onion onion onion onion)

#|
(run 1 q
  (alwayso)
  fail)
;; no value
|#

(run 1 q
  (== 'garlic q)
  succeed
  (== 'onion q))
;; '()

#|
(run 1 q
  (== 'garlic q)
  (alwayso)
  (== 'onion q))
;; no value
|#

(run 1 q
  (conde
    [(== 'garlic q) (alwayso)]
    [(== 'onion q)])
  (== 'onion q))
;; '(onion)

#|
(run 2 q
  (conde
    [(== 'garlic q) (alwayso)]
    [(== 'onion q)])
  (== 'onion q))
;; no value
|#

(run 5 q
  (conde
    [(== 'garlic q) (alwayso)]
    [(== 'onion q) (alwayso)])
  (== 'onion q))
;; '(onion onion onion onion onion)

; FRAME 14
(defrel (nevero)
  (nevero))

#|
(run 1 q
  (nevero))
;; no value
|#

(run 1 q
  fail
  (nevero))
;; '()

(run 1 q
  (conde
    [succeed]
    [(nevero)]))
;; '(_0)

(run 1 q
  (conde
    [(nevero)]
    [succeed]))
;; '(_0)

#|
(run 2 q
  (conde
    [succeed]
    [(nevero)]))
;; no value
|#

#|
(run 1 q
  (conde
    [succeed]
    [(nevero)])
  fail)
;; no value
|#

(run 5 q
  (conde
    [(nevero)]
    [(alwayso)]
    [(nevero)]))
;; '(_0 _0 _0 _0 _0)

(run 6 q
  (conde
    [(== 'spicy q) (nevero)]
    [(== 'hot q) (nevero)]
    [(== 'apple q) (alwayso)]
    [(== 'cider q) (alwayso)]))
;; '(apple cider apple cider apple cider)

(run 6 q
  (conde
    [(== 'apple q) (alwayso)]
    [(== 'spicy q) (nevero)]
    [(== 'hot q) (nevero)]
    [(== 'cider q) (alwayso)]))
;; TRS2 : '(apple apple apple apple apple apple)
;; NEW  : '(apple cider apple cider apple cider)

; FRAME 24
(defrel (very-recursiveo)
  (conde
    [(nevero)]
    [(very-recursiveo)]
    [(alwayso)]
    [(very-recursiveo)]
    [(nevero)]))

(length (run 1000000 q (very-recursiveo)))