;; iDFS
> (run 12 q
    (fresh (x)
      (conde
        ((== 'a x))
        ((== 'b x))
        ((== 'c x))
        ((== 'd x)))
      (repeato x q)))
'((a) (a a) (b) (a a a)
  (a a a a) (b b)
  (a a a a a) (c)
  (a a a a a a) (b b b)
  (a a a a a a a) (d))
