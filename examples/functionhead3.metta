(= (trueOnly $x True) $x)
(= (trueOnly $x False) (empty))

(= (in $x $L)
   (trueOnly $x (memberfast $x $L)))

(= (myplus (in $X (1 2 3)) 
           (in $Y (2 3)))
   (in (+ $X $Y) (3 4 5)))

;fine:
!(test (collapse (myplus 1 3)) (4))
;output out of range:
!(test (collapse (myplus 3 3)) ())
;input out of range:
!(test (collapse (myplus 3 4)) ())
