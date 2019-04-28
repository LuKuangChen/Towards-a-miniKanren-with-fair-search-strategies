> (run 9 q
    (fresh (x)
      (conde
        [(== 'a x)]
        [(== 'b x)]
        [(== 'c x)])
      (repeato x q)))
;; fair conj (i.e. our BFS, Silvija's BFS)
'((a) (b) (c)
  (a a) (b b) (c c)
  (a a a) (b b b) (c c c))
;; unfair conj (i.e. iDFS, fDFS)
'((a)
  (a a) (b)
  (a a a) (c)
  (a a a a) (b b)
  (a a a a a) (c c))