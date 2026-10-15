; A plain parameter can infer its type from body use. A destructured field
; needs evidence from the head's own type to discharge its runtime check.
; Both clauses below compile to guard-free multiplication: tvf infers its
; parameters, while tvf2's declaration types the fields of its head pattern.
; --strict rejects residual runtime checks, so compilation is the assertion.

(= (tvf  $s $c)       (* $s $c))
(: tvf2 (-> (stv Number Number) Number))
(= (tvf2 (stv $s $c)) (* $s $c))

!(test (tvf 2 3) 6)
!(test (tvf2 (stv 2 3)) 6)

; A pattern headed by a uniquely declared constructor takes its field types
; from that declaration instead, and needs no type for the scrutinee to do it:
; in a head pattern, in a let/let* binder, and in a case branch.
(: Pair (-> Number Number PairT))

(= (pair-head (Pair $a $b)) (* $a $b))
(= (pair-let  $p) (let (Pair $a $b) $p (* $a $b)))
(= (pair-let* $p) (let* (((Pair $a $b) $p)) (* $a $b)))
(= (pair-case $p) (case $p (((Pair $a $b) (* $a $b)))))

!(test (pair-head (Pair 2 3)) 6)
!(test (pair-let  (Pair 2 3)) 6)
!(test (pair-let* (Pair 2 3)) 6)
!(test (pair-case (Pair 2 3)) 6)
