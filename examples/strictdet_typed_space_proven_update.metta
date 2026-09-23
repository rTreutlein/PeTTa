!(import! &self (library lib_builtin_types))

; Rows the compiler proves against the schema are stored without the runtime
; re-check; the payload is stored raw, so a variable first field is data and
; the update stays det under --strict-det.
(: Key Type)
(: key (-[det]-> Number Key))
(: &rows (SpaceOf (Number Key Atom)))
!(bind! &rows (new-space))

(: tag-of (-[det]-> Key Atom))
(= (tag-of $_key) tagged)

(: put! (-[det]-> Number Key Atom))
(= (put! $hash $key)
   (let* (($tag (tag-of $key))
          ($_row (add-atom &rows ($hash $key $tag))))
      ()))

(: drop! (-[det]-> Number Key Atom))
(= (drop! $hash $key)
   (let* (($tag (tag-of $key))
          ($_row (remove-atom &rows ($hash $key $tag))))
      ()))

; An Atom-typed first field: data, not a call, in the stored row.
(: &heads (SpaceOf (Atom Key)))
!(bind! &heads (new-space))

(: put-head! (-[det]-> Key Atom))
(= (put-head! $key)
   (let* (($head (tag-of $key))
          ($_row (add-atom &heads ($head $key))))
      ()))

!(put! 1 (key 1))
!(put! 2 (key 2))
!(drop! 1 (key 1))
!(test (collapse (match &rows ($h $k $t) ($h $t))) ((2 tagged)))

!(put-head! (key 3))
!(test (collapse (match &heads ($h $k) $k)) ((key 3)))
