;Loading a Prolog builtin predicate that follows function form convention:
!(import_prolog_function re_replace)
;Loading a Prolog builtin that does not use the last arg as output variable, but the second one:
!(import_prolog_function read_file_to_string)
;and further expects the output to be some instantiated options list:
(= (read_file_to_string_functionized $filename $options) (let $options (read_file_to_string $filename $text) $text))
;which in most cases we can ignore:
(= (read_file_to_string_functionized $filename) (read_file_to_string_functionized $filename ()))
;Loading a PeTTa-internal predicate that follows function form convention:
!(import_prolog_function process_metta_string)

;Process the file string without the !(test (fib 30) 832040) call:
!(process_metta_string (re_replace "\\!\\(test \\(fib 30\\) 832040\\)" "" (read_file_to_string_functionized "./examples/fib.metta")))

;Do our own call:
!(test (fib 10) 55)
