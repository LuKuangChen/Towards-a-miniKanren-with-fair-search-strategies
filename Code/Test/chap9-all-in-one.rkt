#lang racket
#| test cases from Chapter 9 of TRS2|#
(require "./trs2-arith.rkt")
(require "../mk-monads.rkt")

(define SS DFSf)

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(run SS 1 q
  (conda
    [fail succeed]
    [succeed fail]))
;'()

(run SS 1 q
  (conda
    [fail succeed]
    [succeed succeed]))
;'(_0)

(run SS 1 q
  (conda
    [succeed fail]
    [succeed succeed]))
;'()

(run SS 1 q
  (conda
    [succeed succeed]
    [succeed fail]))
;'(_0)

(run* SS x
  (conda
    [(== 'olive x) succeed]
    [succeed (== 'oil x)]))
;'(olive)

(run* SS x
  (conda
    [(== 'virgin x) fail]
    [(== 'olive x) succeed]
    [succeed (== 'oil x)]))
;'()

(run* SS q
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (conda
      [(== 'split x) (== x y)]
      [succeed succeed])))
;'()

(run* SS q
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

(run* SS x
  (conda
    [(not-pastao x) fail]
    [(== 'spaghetti x) succeed]))
;'(spaghetti)

(run* SS x
  (== 'spaghetti x)
  (conda
    [(not-pastao x) fail]
    [(== 'spaghetti x) succeed]))
;'()

#|
(run* BFS q
  (conda
    [(alwayso) succeed]
    [succeed fail]))
; no value
|#

(run* SS q
  (condu
    [(alwayso) succeed]
    [succeed fail]))
;'(_0)

#|
(run* BFS q
  (condu
    [succeed (alwayso)]
    [succeed fail]))
; no value
|#

#|
(run* BFS q
  (conda
    [(alwayso) succeed]
    [succeed fail])
  fail)
; no value
|#

(run* SS q
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

(run* SS x
  (onceo (teacupo x)))
;'(tea)

(run* SS r
  (conde
    [(teacupo r) succeed]
    [(== #f r) succeed]))
;'(#f tea cup)

(run* SS r
  (conda
    [(teacupo r) succeed]
    [succeed (== #f r)]))
;'(tea cup)

(run* SS r
  (== #f r)
  (conda
    [(teacupo r) succeed]
    [succeed (== #f r)]))
;'(#f)

(run* SS r
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

(run* SS x
  (bumpo '(1 1 1) x))

;'((1 1 1) (0 1 1) (1 0 1) (0 0 1) (1 1) (0 1) (1) ())

;; frame 27
(defrel (gen&test+o i j k)
  (onceo
   (fresh (x y z)
     (pluso x y z)
     (== i x)
     (== j y)
     (== k z))))

(run* SS q
  (gen&test+o '(0 0 1) '(1 1) '(1 1 1)))
;'(_0)

#|
(run BFS 1 q
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

(run* SS s
  (enumerate+o s '(1 1)))
;'(((1 1) (1 1) (0 1 1))
;  ((1 1) () (1 1))
;  (() (1 1) (1 1))
;  ((1 1) (0 1) (1 0 1))
;  ((0 1) (1 1) (1 0 1))
;  ((0 1) () (0 1))
;  ((1) (1 1) (0 0 1))
;  (() (0 1) (0 1))
;  ((1) (1) (0 1))
;  ((0 1) (0 1) (0 0 1))
;  ((1) () (1))
;  (() (1) (1))
;  ((1 1) (1) (0 0 1))
;  ((1) (0 1) (1 1))
;  (() () ())
;  ((0 1) (1) (1 1)))


(run SS 1 s
  (enumerate+o s '(1 1 1)))
;'(((1 1 1) (1 1 1) (0 1 1 1)))

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
(run* SS s
  (enumerateo *o s '(1 1)))
;'(((1 1) (1) (1 1))
;  ((1) (1 1) (1 1))
;  (() (1 1) ())
;  ((0 1) (1 1) (0 1 1))
;  ((1 1) () ())
;  ((1 1) (0 1) (0 1 1))
;  ((0 1) (1) (0 1))
;  ((1) (0 1) (0 1))
;  (() (0 1) ())
;  ((0 1) (0 1) (0 0 1))
;  ((0 1) () ())
;  ((1) (1) (1))
;  (() (1) ())
;  ((1) () ())
;  (() () ())
;  ((1 1) (1 1) (1 0 0 1)))