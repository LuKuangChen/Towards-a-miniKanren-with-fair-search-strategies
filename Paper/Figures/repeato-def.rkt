(defrel (repeato x out)
  (conde
    ((== `(,x) out))
    ((fresh (res)
       (== `(,x . ,res) out)
       (repeato x res)))))
