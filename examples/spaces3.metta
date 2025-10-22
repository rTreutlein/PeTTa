(wu)
(wu 42)

!(test (collapse (match &self ($1) ($1))) ((wu)))
!(test (collapse (match &self ($1) (hu $1))) ((hu wu)))
!(test (collapse (match &self ($1) $1)) (wu))
!(test (msort (collapse (match &self $1 $1))) (msort (collapse (get-atoms &self))))
;!(match &self ($1) (hu $1))
