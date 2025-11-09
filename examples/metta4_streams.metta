!(import! &self ../lib/lib_metta4)

;nondeterministically returning integers withing range:
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

;add every range item to space &s1 using metta4's for:
!(for $x (range 1 5)
      (|-> ($x) 
           (add-atom &s1 (num $x))))

;add first range item to space &s1 using metta4's for:
!(sel $x (range 1 5)
         (|-> ($x) 
              (add-atom &s2 (num $x))))

!(test (collapse (get-atoms &s1))
       ((num 1) (num 2) (num 3) (num 4)))

!(test (collapse (get-atoms &s2))
       ((num 1)))
