!(test (if (and (or $x True) $y) ($x $y))
       ((true true) (false true)))
