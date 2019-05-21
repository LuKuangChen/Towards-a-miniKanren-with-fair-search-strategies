;; BFSser (fair conj)
> (run 12 q
    (fresh (xs)
      (conde
        ((repeato 'a xs))
        ((repeato 'b xs)))
      (repeato xs q)))
'(((a)) ((b))
  ((a) (a)) ((b) (b))
  ((a a)) ((b b))
  ((a) (a) (a)) ((b) (b) (b))
  ((a a) (a a)) ((b b) (b b))
  ((a a a)) ((b b b)))
