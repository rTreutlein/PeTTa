(= (p1) (+ 1))
(= (p2) (+ 2))

(: mreduce (-> Expression Atom))
(= (mreduce $arg) (quote (((reduce $arg $out)) $out)))

!(set_hook mreduce)

(: compose (-> Expression Atom Atom))
(= (compose (cons $f $rest) $args)
   (if (== $rest ())
       (let $x (cons $f $args) (mreduce $x))
       ($f (compose $rest $args))))

!(compose (p1 p2) (1))
