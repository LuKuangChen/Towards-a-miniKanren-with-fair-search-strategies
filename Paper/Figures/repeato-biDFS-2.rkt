> (run 16 q
    (conde
      [(repeato 'a q)]
      [(repeato 'b q)]
      [(repeato 'c q)]
      [(repeato 'd q)]))
;; biDFS (almost fair disj)
'((a) (b) (c) (d)
  (a a) (b b) (c c) (d d)
  (a a a) (b b b) (c c c) (d d d)
  (a a a a) (b b b b) (c c c c) (d d d d))