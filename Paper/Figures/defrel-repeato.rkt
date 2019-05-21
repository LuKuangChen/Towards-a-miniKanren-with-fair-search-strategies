(defrel (repeato x out)
  (conde
    ((== '() out))
    ((fresh (res)
       (== `(,x . ,res) out)
       (repeato x res)))))
