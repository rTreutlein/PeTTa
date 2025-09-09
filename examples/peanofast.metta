(= (expandK $expression $n)
   (if (== $n 0)
       done
       (let $temp1 (add-atom &self (num $expression))
            (expandK (Z $expression) (- $n 1)))))

(= (indexing-demo-peano $K)
   (let $g (expandK Z $K)
        (match &self (num $1) $1)))

!(indexing-demo-peano 3000)
