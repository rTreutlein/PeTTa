;generator: (separate non-det return of each item)
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

!(let $temp (let $x (range 1 100000)
                 (mork_add-atom &mork (friend sam $x)))
      (empty))

!(mork_add-atom &mork (exec 0 (, (friend sam $x)) 
                              (, (freund sam $x))))
!(mork_exec &mork)

!(test (length (collapse (mork_match &mork (freund sam $x) (freund sam $x)))) 99999)
