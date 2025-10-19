(= (f $L $a $b)
   (let $result (+ $a $b)
        (append ($result) $L)))

;Match does not automatically interpret the returned pattern as code:
;!(match &self (= (f 40.7 2) $x) $x)

;However that can be done too:
(= (eval $body)
   (let* (($a   (add-atom &self (= (myfunc) $body)))
          ($res (myfunc))
          ($r   (remove-atom &self (= (myfunc) $body))))
         $res))

!(test (eval (match &self (= (f (42) 40.7 2) $x) $x))
       (42.7 42))
