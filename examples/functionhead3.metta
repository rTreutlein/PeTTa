(= (qualify $x True) $x)

(= (in $x $L)
   (qualify $x (memberfast $x $L)))

(= (myplus (in $X (1 2 3)) 
           (in $Y (2 3)))
   (in (+ $X $Y) (3 4 5)))

;fine:
!(test (collapse (myplus 1 3)) (4))
;output out of range:
!(test (collapse (myplus 3 3)) ())
;input out of range:
!(test (collapse (myplus 3 4)) ())
;what can be reached when adding $X to 3:
!(test (collapse (myplus $x 3)) (4 5))
;what can be reached when adding $X to $Y:
!(test (collapse (myplus $x $y)) (3 4 4 5 5))
