(: a A)
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))

; An untyped positional head pattern gives no evidence about its fields.
(= (h ($x)) (only-a $x))
!(test (collapse (h (a))) ((ok a)))
!(test (repr (catch (h (b)))) "(Error (literal_type_mismatch b A) typecheck)")

; The same obligation survives nested destructuring.
(= (nested (($x))) (only-a $x))
!(test (collapse (nested ((a)))) ((ok a)))
!(test (repr (catch (nested ((b))))) "(Error (literal_type_mismatch b A) typecheck)")

; Plain-parameter inference moves the requirement to the caller; unresolved
; values there use the same residual error as destructured values.
(= (g $x) (only-a $x))
!(test (g a) (ok a))
!(test (repr (catch (g (car-atom (b))))) "(Error (literal_type_mismatch b A) typecheck)")
