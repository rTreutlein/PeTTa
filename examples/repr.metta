!(test (repr 42) "42")
;TODO works but test fails since parser does not deal with escape symbolss:
!(test (repr "42") ""42"")
;TODO already works but parser does not like ( and spaces in strings yet:
;!(test (repr (A (B C))) "(A (B C))")
