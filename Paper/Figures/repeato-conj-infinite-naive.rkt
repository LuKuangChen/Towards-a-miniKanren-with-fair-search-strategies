;; naively fair conj
> (run 6 q
    (fresh (xs)
      (conde
        ((repeato 'a xs))
        ((repeato 'b xs)))
      (repeato xs q)))
'(((a)) ((b))
  ((a a)) ((b b))
  ((a a a)) ((b b b)))
