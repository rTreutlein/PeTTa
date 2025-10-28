(: apply (-> Expression %Undefined% %Undefined%))
(= (apply (lambda $var $body) $arg)
   (eval (let $var $arg $body)))

(= (applyL1)
   (apply (lambda $x (+ $x 1)) 2))

(= (applyL2)
   (apply (lambda ($x $y) (+ $x $y)) (2 7)))

!(test (applyL1) 3)
!(test (applyL2) 9)
