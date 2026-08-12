; Representative outer control-flow shape used by PeTTaChainer's
; query-needs-temp-rule?.
; The nonempty guard makes car/cdr safe, reflection and alpha membership
; return proper Booleans, and every branch constructs a Bool exactly once.
(: query-shape? (-[det]-> Expression Bool))
(= (query-shape? $term)
   (if (and
          (is-expr $term)
          (not (== $term ())))
      (let $head (car-atom $term)
         (and
            (not (is-var $head))
            (and
               (is-alpha-member $head (And Or LikelierThan))
               (not (== (cdr-atom $term) ())))))
      false))

!(test (query-shape? (And a b)) true)
!(test (query-shape? (Other a b)) false)
!(test (query-shape? ()) false)
