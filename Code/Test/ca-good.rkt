#lang racket
(require "../mk-3-1-ca.rkt")
(require racket/trace)
#| constant-time append should have great advantage |#

(defrel (relo)
  (conde
    [(relo)]
    [(relo)]
    [succeed]))

(length
 (run 1000000 q
   (relo)))
