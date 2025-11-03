(= (mysets)
   (superpose ((set 1 (superpose (a b c)))
               (set 2 (superpose (d e f)))
               (set 3 (superpose (a b))))))

!(let $x (mysets)
      (add-atom &setspace $x))

!(test (collapse (get-atoms &setspace))
       ((set 1 a) (set 1 b) (set 1 c) (set 2 d) (set 2 e) (set 2 f) (set 3 a) (set 3 b)))
