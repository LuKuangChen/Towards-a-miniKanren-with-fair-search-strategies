;; DFSbi (unfair conj)
> (run 12 q
    (fresh (x)
      (conde
        ((== 'a x))
        ((== 'b x))
        ((== 'c x))
        ((== 'd x)))
      (repeato x q)))
'((a) (a a) (c) (a a a)
  (a a a a) (c c)
  (a a a a a) (b)
  (a a a a a a) (c c c)
  (a a a a a a a) (d))