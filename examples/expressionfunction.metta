(: wu (-> Number Expression Expression))
(= (wu $a $b)
   (42 $a $b))

!(test (wu (+ 2 4) (+ 4 2)) (42 6 (quote (+ 4 2))))
