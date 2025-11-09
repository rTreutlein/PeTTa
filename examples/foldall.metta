(= (f) 2)
(= (f) 3)

(= (g 1) 2)
(= (g 2) 3)

(= (merge $A $B) (+ $A $B))

;agg function plus arg-free generator
!(test (foldall merge (f) 0)
       5)

;agg function plus arg-ful generator
!(test (foldall merge (g $x) 0)
       5)

;agg lambda plus arg-free generator
!(test (let $agglambda (|-> ($x $y) (+ $x $y))
            (foldall $agglambda (f) 0))
       5)

;agg lambda plus arg-ful generator
!(test (let $agglambda (|-> ($x $y) (+ $x $y))
            (foldall $agglambda (g $x) 0))
       5)

;agg lambda plus arg-ful generator
!(test (let $agglambda (|-> ($x $y) (+ $x $y))
            (foldall $agglambda (g $x) 0))
       5)

;agg lambda plus lambda arg-free generator
!(test (let* (($agglambda (|-> ($x $y) (+ $x $y)))
              ($genlambda (|-> ($x) (f))))
            (foldall $agglambda ($genlambda $x) 0))
       5)

;agg lambda plus lambda arg-ful generator
!(test (let* (($agglambda (|-> ($x $y) (+ $x $y)))
              ($genlambda (|-> ($x) (g $x))))
            (foldall $agglambda ($genlambda $x) 0))
       5)

