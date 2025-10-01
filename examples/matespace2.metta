(= (add-atom-no-duplicate $Space $Atom)
   (if (== () (collapse (once (match $Space $Atom $Atom))))
       (add-atom $Space $Atom)
       (empty)))

;Rewrite rules:
(= (expand)
   (case (match &self (num $t) $t) 
         (($t ((add-atom-no-duplicate &self (num (M $t)))
               (add-atom-no-duplicate &self (num (W $t))))))))

(= (mate)
   (case (match &self (num (M $t)) $t)
         (($t (case (once (match &self (num (W $t)) $t))
                    (($t (add-atom-no-duplicate &self (num (C $t))))))))))

;Apply expand and mate K times:
(= (rewriteK $n)
   (if (== $n 0)
       done
       (let* (($temp1 (expand))
              ($temp2 (mate)))
             (rewriteK (- $n 1)))))

;Apply the rewrites K times, then match:
(= (mate-space-demo $K)
   (let* (($s (add-atom &self (num Z)))
          ($g (rewriteK $K)))
          (match &self (num $1) (num $1))))

!(test (length (collapse (mate-space-demo 300))) 1077002) 
