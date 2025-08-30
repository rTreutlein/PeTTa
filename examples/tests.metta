(= (prog1 $Y) (let $X $Y (collapse (superpose (12 (+ $X 4))))))

(= (prog2 $Y) (let $list (let $L (1 2 3) (collapse (superpose $L))) (superpose $list)))

(= (prog3 $x) (if (== $x 2) (let $z (superpose ((if (< $x 10) (superpose ((42 43))) 43))) $z) (let $z 4 $z)))

(= (prog4) (collapse ((prog1 42) (prog2 42) (prog3 2))))

!(test (prog4) (((12 46) 1 (42 43)) ((12 46) 2 (42 43)) ((12 46) 3 (42 43))))
