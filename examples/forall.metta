(= (f) 1)
(= (f) 2)

(= (g 1) 1)
(= (g 2) 2)

(= (P $X) (< $X 2))

;Arg-free generator function plus check function
!(test (forall (f) P)
       false)

;Arg-ful generator function plus check function
!(test (forall (g $x) P)
       false)

;Arg-ful generator lambda plus check function
!(test (let $genlambda (|-> ($x) (g $x))
            (forall ($genlambda $x) P))
       false)

;Arg-ful generator function plus check lambda
!(test (let $checklambda (|-> ($v) (< $v 2))
            (forall (g 2) $checklambda))
       false)

;Arg-ful generator function plus check lambda
!(test (let $checklambda (|-> ($v) (< $v 2))
            (forall (g 1) $checklambda))
       true)

;Arg-ful generator function plus check lambda
!(test (let $checklambda (|-> ($v) (< $v 2))
            (forall (g 2) $checklambda))
       false)

;Arg-ful generator lambda plus check lambda
!(test (let* (($checklambda (|-> ($v) (< $v 2)))
              ($genlambda (|-> ($x) (g $x))))
             (forall ($genlambda $x) $checklambda))
       false)
