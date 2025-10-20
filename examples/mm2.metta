;Add item to space without duplicates (cheap exists check due to 'once' despite 'collapse')
(= (add-atom-no-duplicate $Space $Atom)
   (if (== () (collapse (once (match $Space $Atom $Atom))))
       (add-atom $Space $Atom)
       (empty)))

;Execute sinks:
(= (MM2_ApplyRuleBody ()) True)
(= (MM2_ApplyRuleBody $sink)
   (let* (($headop (car-atom $sink))
          ($rest (cdr-atom $sink)))
         (and (case $headop
                    (((＋ $inside) (add-atom-no-duplicate &PLUS $inside))
                     ((－ $inside) (add-atom-no-duplicate &MINUS $inside))
                     (($Else)     (add-atom-no-duplicate &PLUS $inside))))
              (MM2_ApplyRuleBody $rest))))

;MM2 space transform based on (Head Body) rewrite rule expression
(= (MM2 $expression)
   (let ($source $sink) $expression
        (let* (($zero1 (add-atom &PLUS (dummy)))  ;needed for now until dynamic dispatch
               ($zero2 (add-atom &MINUS (dummy))) ;checks arity too (planned extension)
               ($a (collapse (let $sink (match &self $source $sink)
                                  (if (or (== O (car-atom $sink))
                                          (== , (car-atom $sink)))
                                      (MM2_ApplyRuleBody (cdr-atom $sink))
                                      (nope)))))
               ($b (collapse (let $x (get-atoms &PLUS)
                                  (add-atom-no-duplicate &self $x))))
               ($c (collapse (let $y (get-atoms &MINUS)
                                  (remove-atom &self $y))))
               ($r (collapse (let $z (get-atoms &MINUS)
                                  (remove-atom &MINUS $z))))
               ($s (collapse (let $w (get-atoms &PLUS)
                                  (remove-atom &PLUS $w)))))
              true)))

;Generator: (separate non-det return of each item)
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

;Spacecount: item count in space without list building
(= (countitem) (let $x (get-atoms &self) 1))
(= (merge $a $b) (+ $a $b))
(= (spacecount) (foldall merge (countitem) 0))
