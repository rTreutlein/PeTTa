(= (f) 2)
(= (f) 3)

(= (merge $A $B) (+ $A $B))

!(test (foldall merge (f) 0) 5)
