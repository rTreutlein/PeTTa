(foo 1)
(foo 2)
(foo 42 42)
(foo (42 42))

!(bar 42)
!(bar 43)

(= (answer) 42)

!(test (space (collapse (get-atoms &self)) (answer))
       (space ((foo 1) (foo 2) (foo (42 42)) (foo 42 42)) 42))
