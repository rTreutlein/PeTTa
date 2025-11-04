(: apply (-> Expression %Undefined% %Undefined%))
(= (apply (lambda $var $body) $arg)
   (eval (let $var $arg $body)))

(= (applyL1)
   (apply (lambda $x (+ $x 1)) 2))

(= (applyL2)
   (apply (lambda ($x $y) (+ $x $y)) (2 7)))

!(test (applyL1) 3)
!(test (applyL2) 9)

!(test (maplist (|-> ($a) (+ 1 $a)) (1 2 3))
       (2 3 4))

!(test ((|-> ($acc $e) (or (== 1 $e) $acc)) False 1)
        True)
