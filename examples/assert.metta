(= (assertEqual $A $B)
   (assert (== $A $B)))

(= (assertAlphaEqual $A $B)
   (assert (=alpha $A $B)))

!(test (assertEqual (+ 1 2) (- 6 3)) True)
!(test (assertAlphaEqual (quote (+ $x $y)) (quote (+ $a $b))) True)
