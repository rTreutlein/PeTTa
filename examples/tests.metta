(= (prog1 $Y) (let $X $Y (collapse (superpose (12 (plus $X 4))))))

(= (prog2 $Y) ((superpose (let $L (1 2 3) (collapse (superpose $L))))))

(= (prog3 $x) (if (eq $x 2) (let $z (superpose ((if (lt $x 10) (superpose ((42 43))) 43))) $z) (let $z 4 $z))) 

(= (prog4) ((prog1 42) (prog2 42) (prog3 2)))

!(eq (prog4) ((12 46) (1) (42 43)))
