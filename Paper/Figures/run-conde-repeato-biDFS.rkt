> (run 12 q
    (conde
      [(repeato 'a q)]
      [(repeato 'b q)]
      [(repeato 'c q)]))
'(() ()
  (b) ()
  (b b) (a)
  (b b b) (c)
  (b b b b) (a a)
  (b b b b b) (c c))