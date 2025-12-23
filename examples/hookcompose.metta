(: mymacro (-> Expression Expression))
(= (mymacro $arg)
   (quote (((cons 42 $arg $out)) $out)))

!(set_hook mymacro)

!(test (mymacro (43)) (42 43))
