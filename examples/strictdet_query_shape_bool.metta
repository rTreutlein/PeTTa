; Representative outer control-flow shape used by PeTTaChainer's
; query-needs-temp-rule?.
; The nonempty guard makes decons exactly-one, its result is an exact
; two-field list, and every branch constructs a Bool exactly once.
(: statement-children (-[det]-> Expression Expression))
(= (statement-children $items) $items)

(: compound-direct-query-bundle? (-[det]-> Expression Bool))
(= (compound-direct-query-bundle? $items) (not (== $items ())))

(: query-shape? (-[det]-> Expression Bool))
(= (query-shape? $term)
   (if (and
          (is-expr $term)
          (not (== $term ())))
      (let ($head $tail) (decons $term)
         (if (is-var $head)
            false
            (if (is-alpha-member $head (And Or LikelierThan))
               (compound-direct-query-bundle?
                  (statement-children $tail))
               false)))
      false))

!(test (query-shape? (And a b)) true)
!(test (query-shape? (Other a b)) false)
!(test (query-shape? ()) false)
