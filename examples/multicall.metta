(= (mycalc $x $y)
   (+ $x $y))
(= (mycalc $x $y)
   (- $x $y))

(= (wu)
   (collapse (mycalc 1 2)))

!(test (wu) (3 -1))
