#lang racket
#| chap9 |#
(require "../trs2-arith.rkt")
(require "../mk.rkt")

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(run 1 q
  (conda
    [fail succeed]
    [succeed fail]))
;'()

(run 1 q
  (conda
    [fail succeed]
    [succeed succeed]))
;'(_0)

(run 1 q
  (conda
    [succeed fail]
    [succeed succeed]))
;'()

(run 1 q
  (conda
    [succeed succeed]
    [succeed fail]))
;'(_0)

(run* x
  (conda
    [(== 'olive x) succeed]
    [succeed (== 'oil x)]))
;'(olive)

(run* x
  (conda
    [(== 'virgin x) fail]
    [(== 'olive x) succeed]
    [succeed (== 'oil x)]))
;'()

(run* q
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      [(== 'split x) (== x y)]
      [succeed succeed])))
;'()

(run* q
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      [(== x y) (== 'split x)]
      [succeed succeed])))
;'(_0)

;; frame 11
(defrel (not-pastao x)
  (conda
    [(== 'pasta x) fail]
    [succeed succeed]))

(run* x
  (conda
    [(not-pastao x) fail]
    [(== 'spaghetti x) succeed]))
;'(spaghetti)

(run* x
  (== 'spaghetti x)
  (conda
    [(not-pastao x) fail]
    [(== 'spaghetti x) succeed]))
;'()

#|
(run* q
  (conda
    [(alwayso) succeed]
    [succeed fail]))
; no value
|#

(run* q
  (condu
    [(alwayso) succeed]
    [succeed fail]))
;'(_0)

#|
(run* q
  (condu
    [succeed (alwayso)]
    [succeed fail]))
; no value
|#

#|
(run* q
  (conda
    [(alwayso) succeed]
    [succeed fail])
  fail)
; no value
|#

(run* q
  (condu
    [(alwayso) succeed]
    [succeed fail])
  fail)
;'()

;; frame 20
(defrel (teacupo t)
  (conde
    [(== 'tea t)]
    [(== 'cup t)]))

;; frame 21
(defrel (onceo g)
  (condu
   [g succeed]
   [succeed fail]))

(run* x
  (onceo (teacupo x)))
;'(tea)

(run* r
  (conde
    [(teacupo r) succeed]
    [(== #f r) succeed]))
;'(#f tea cup)

(run* r
  (conda
    [(teacupo r) succeed]
    [succeed (== #f r)]))
;'(tea cup)

(run* r
  (== #f r)
  (conda
    [(teacupo r) succeed]
    [succeed (== #f r)]))
;'(#f)

(run* r
  (== #f r)
  (conda
    [(teacupo r) succeed]
    [(== #f r) succeed]
    [succeed fail]))
;'(#f)

;; frame 26
(defrel (bumpo n x)
  (conde
    [(== n x)]
    [(fresh (m)
       (minuso n '(1) m)
       (bumpo m x))]))

(run* x
  (bumpo '(1 1 1) x))

;'((1 1 1)
;  (0 1 1)
;  (1 0 1)
;  (0 0 1)
;  (1 1)
;  (0 1)
;  (1)
;  ())

;; frame 27
(defrel (gen&test+o i j k)
  (onceo
   (fresh (x y z)
     (pluso x y z)
     (== i x)
     (== j y)
     (== k z))))

(run* q
  (gen&test+o '(0 0 1) '(1 1) '(1 1 1)))
;'(_0)

#|
(run 1 q
  (gen&test+o
   '(0 0 1) '(1 1) '(0 1 1)))
; no value
|#

; frame 43
(defrel (enumerate+o r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (pluso i j k)
    (gen&test+o i j k)
    (== `(,i ,j ,k) r)))

(run* s
  (enumerate+o s '(1 1)))
;'(((1 1) () (1 1))
;  (() (1 1) (1 1))
;  ((0 1) () (0 1))
;  (() (0 1) (0 1))
;  ((1) (1) (0 1))
;  ((1) () (1))
;  (() (1) (1))
;  (() () ())
;  ((1) (0 1) (1 1))
;  ((1 1) (1 1) (0 1 1))
;  ((1 1) (0 1) (1 0 1))
;  ((0 1) (1 1) (1 0 1))
;  ((0 1) (0 1) (0 0 1))
;  ((1) (1 1) (0 0 1))
;  ((0 1) (1) (1 1))
;  ((1 1) (1) (0 0 1)))


(run 1 s
  (enumerate+o s '(1 1 1)))
;'(((1 1 1) () (1 1 1)))

; frame 58
#; #| new |#
(defrel (enumerate+o r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (pluso i j k)
    (onceo
     (fresh (x y z)
       (pluso x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== `(,i ,j ,k) r)))

; frame 59
(defrel (enumerateo op r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (onceo
     (fresh (x y z)
       (op x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== `(,i ,j ,k) r)))

#| the extra test |#
(run* s
  (enumerateo *o s '(1 1)))
;'(((1) (1 1) (1 1))
;  ((1 1) (1) (1 1))
;  (() (1 1) ())
;  ((1 1) () ())
;  ((1) (0 1) (0 1))
;  ((0 1) (1) (0 1))
;  (() (0 1) ())
;  ((0 1) () ())
;  ((1) (1) (1))
;  ((0 1) (1 1) (0 1 1))
;  (() (1) ())
;  ((1) () ())
;  (() () ())
;  ((0 1) (0 1) (0 0 1))
;  ((1 1) (0 1) (0 1 1))
;  ((1 1) (1 1) (1 0 0 1)))