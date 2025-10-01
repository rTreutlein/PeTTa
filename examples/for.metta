(= (forall $L $P)
   (if (== () (collapse (once (let $x (superpose $L)
                                   (if (not ($P $x))
                                       False
                                       (empty))))))
       True
       False))

(= (P $X) (> $X 2))
!(forall (5 6 3 4 1 25) P)
