(= (add-atom-no-duplicate $Space $Atom)
   (if (== () (collapse (match $Space $Atom $Atom)))
       (add-atom $Space $Atom)
       (empty)))

; Rewrite rules:
(= (expand)
   (case (match &self (num $t) $t) 
         (($x ((add-atom-no-duplicate &self (num (M $x)))
               (add-atom-no-duplicate &self (num (W $x))))))))

(= (mate)
   (case (match &self (num (M $t)) $t)
         (($x (case (match &self (num (W $t)) $t)
                    (($x (add-atom-no-duplicate &self (num (C $x))))))))))

; Apply expand and mate K times:
(= (rewriteK $n)
   (if (== $n 0)
       done
       (let* (($temp1 (expand))
              ($temp2 (mate)))
             (rewriteK (- $n 1)))))

; Apply the rewrites K times, then match:
(= (mate-space-demo $K)
   (let* (($s (add-atom &self (num Z)))
          ($g (rewriteK $K)))
          (match &self (num $1) (num $1))))

!(test (length (collapse (mate-space-demo 300))) 1077002) 
