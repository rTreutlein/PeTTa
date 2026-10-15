; A visibly invalid call is still rejected by static analysis. When the
; fields cannot be resolved statically, the body retains residual checks:
; SWI-Prolog must not interpret these one-character strings as character codes.
(= (tvf2 (stv $s $c)) (* $s $c))

!(tvf2 (stv "a" "b"))
