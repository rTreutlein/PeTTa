; Integration fixture: load after examples/type_destructured_recursive_guard.metta
; on top of #231. Main's generic recursive fallback cannot enforce this call.
!(test (repr (catch (collapse (each only-a (a (b end))))))
       "(Error (literal_type_mismatch b A) typecheck)")
