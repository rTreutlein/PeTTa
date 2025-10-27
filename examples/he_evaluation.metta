!(import! &self lib_he)

!(test (for-each-in-atom (1 3 5 62 2 5) println!)
       (true true true true true true))
