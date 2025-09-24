!(test (repr 42) "42")
!(test (repr "42") "\"42\"")
!(test (repr (A (B C))) "(A (B C))")
