(= (compilecons42 $arg)
   (cons 42 $arg))

!(add-translator-rule! compilecons42)

!(test (compilecons42 (43)) (42 43))
