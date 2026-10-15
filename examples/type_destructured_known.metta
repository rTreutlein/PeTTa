(: a A)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))

; Plain-parameter inference carries the requirement to the caller.
(= (g $x) (only-a $x))
!(test (g a) (ok a))

; A declared head type discharges the field check statically.
(: typed-h (-> (List A) %Undefined%))
(= (typed-h ($x)) (only-a $x))
!(test (typed-h (a)) (ok a))

; Constructor declarations also provide real field evidence.
(: Box (-> A Boxed))
(= (boxed (Box $x)) (only-a $x))
!(test (boxed (Box a)) (ok a))
