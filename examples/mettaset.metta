!(let $x (superpose ((set 1 (superpose (a b c)))
                     (set 2 (superpose (d e f)))
                     (set 3 (superpose (a b)))))
      (add-atom &self $x))

!(test (collapse (match &self (set $x $y) (set $x $y)))
       ((set 1 a) (set 1 b) (set 1 c) (set 2 d) (set 2 e) (set 2 f) (set 3 a) (set 3 b)))
