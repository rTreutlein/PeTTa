(= (f $X $Y)
   (append ($X) $Y))

!(test (let (f $Head $Tail) (1 2 3 4 5 6) ($Head $Tail))
            (1 (2 3 4 5 6)))
