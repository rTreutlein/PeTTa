!(import! &self "python_import_file.py")
;or !(import! &self absolute/path/to/python_import_file.py)

!(py-call (python_import_file.greet "PeTTa User"))

!(py-call (python_import_file.add 10 20))
