> (run 9 q
    (conde
      [(repeato 'a q)]
      [(repeato 'b q)]
      [(repeato 'c q)]))
;; fair disj
'((a) (b) (c)
  (a a) (b b) (c c)
  (a a a) (b b b) (c c c))