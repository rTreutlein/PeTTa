; A pattern headed by a type name does not constrain its fields. Keep their
; runtime checks rather than assuming types from body use and then dropping
; an inferred shape that cannot be read back as the original pattern.
(: Statement Type)

(= (subj (Statement $s $p)) (* $s $p))

!(test (subj (Statement 2 3)) 6)

; Nested patterns keep residual checks for their unknown fields too:
(= (inner (box (pt $x $y))) (+ $x $y))

!(test (inner (box (pt 2 3))) 5)

; Fields with no typed use remain unconstrained:
(= (fst (pair $a $b)) $a)

!(test (fst (pair 1 "two")) 1)
!(test (fst (pair "one" 2)) "one")
