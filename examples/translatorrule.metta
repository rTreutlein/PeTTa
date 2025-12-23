(: compilecons42 (-> Expression Expression))
(= (compilecons42 $arg)
   (quote (((cons 42 $arg $out)) $out)))

!(add-translator-rule! compilecons42)

!(test (compilecons42 (43)) (42 43))
