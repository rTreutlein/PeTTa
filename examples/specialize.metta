(= (map-flat $f ()) ())
(= (map-flat $f (cons $x $xs)) (cons ($f $x) (map-flat $f $xs)))

(= (map-flat2 (() $f)) ())
(= (map-flat2 ((cons $x $xs) $f)) (cons ($f $x) (map-flat2 ($xs $f))))

!(test (map-flat (+ 1) (1 2 3)) (2 3 4))

!(test (map-flat2 ((1 2 3) (+ 1))) (2 3 4))

(: map-flat3 (-> Expression %Undefined%))
(= (map-flat3 ($f ())) ())
(= (map-flat3 ($f (cons $x $xs))) (cons ($f $x) (map-flat3 ($f $xs))))

(= (p1 $x) (+ 1 $x))

!(test (map-flat3 (p1 (1 2))) (2 3))

(= (wrapper $f $list) (map-flat $f $list))

!(test (wrapper (+ 1) (1 2 3)) (2 3 4))

(= (wrapper2 $f) (id $f))

!(test (wrapper2 (+ 1)) (+ 1))
