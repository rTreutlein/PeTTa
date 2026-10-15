(: a A)
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))
(= (each $f end) end)
(= (each $f ($x $rest)) (($f $x) (each $f $rest)))

; #231 reuses the specialization at every recursive step. Its destructured
; field guard rejects the first invalid element here; the later-element
; regression is tests/destructured_recursive_pr231.metta and requires #231.
!(test (collapse (each only-a (a (a end)))) (((ok a) ((ok a) end))))
!(test (repr (catch (collapse (each only-a (b (a end))))))
       "(Error (literal_type_mismatch b A) typecheck)")
