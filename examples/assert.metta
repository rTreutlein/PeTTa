(= (assertEqual $A $B)
   (assert (== $A $B)))

(= (assertAlphaEqual $A $B)
   (assert (=alpha $A $B)))

!(assertEqual (+ 1 2) (- 6 3))
!(assertAlphaEqual (quote (+ $x $y)) (quote (+ $a $b)))
