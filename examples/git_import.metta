!(import! &self (library lib_prolog))

!(git-import! "https://github.com/rTreutlein/test_metta_lib.git")

!(import! &self (library test))

!(test (mytest 10) 10)
