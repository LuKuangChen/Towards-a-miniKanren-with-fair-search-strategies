;; \BFSser{} (fair conj)
> (run 12 q
    (fresh (x)
      (conde
        ((== 'a x))
        ((== 'b x))
        ((== 'c x))
        ((== 'd x)))
      (repeato x q)))
'((a) (b) (c) (d)
  (a a) (b b) (c c) (d d)
  (a a a) (b b b) (c c c) (d d d))
