import os
from pyswip import Prolog

class MeTTaWrapper:
    def __init__(self, metta_src_path=None):
        self.prolog = Prolog()
        if metta_src_path is None:
            # Assume the src directory is in the same directory as this file
            metta_src_path = os.path.join(os.path.dirname(__file__), '..', 'src')
        self.load_metta(metta_src_path)

    def load_metta(self, path):
        # Load the main MeTTa files
        files_to_load = [
            'filereader.pl',
            'parser.pl',
            'spaces.pl',
            'translator.pl',
            'metta.pl',
            'main.pl'
        ]
        for file in files_to_load:
            file_path = os.path.join(path, file)
            if os.path.exists(file_path):
                self.prolog.consult(file_path)
            else:
                raise FileNotFoundError(f"Could not find {file_path}")

    def load_metta_file(self, file_path):
        """Compile a MeTTa file to Prolog."""
        self.prolog.query(f"load_metta_file('{file_path}')")

    def process_metta_string(self, metta_code):
        """Compile a string of MeTTa code to Prolog."""
        # Note: This assumes process_metta_string is defined in the Prolog code
        # If not, we might need to add it or use load_metta_file with a temp file
        self.prolog.query(f"process_metta_string('{metta_code}')")

    def run(self):
        """Execute the run(R) predicates and return results."""
        results = list(self.prolog.query("run(R)"))
        return [result['R'] for result in results]

    def query(self, query_str):
        # Simple query method, returns list of results
        results = list(self.prolog.query(query_str))
        return results

    def assert_fact(self, fact):
        # Assert a fact
        self.prolog.assertz(fact)

    def retract_fact(self, fact):
        # Retract a fact
        self.prolog.retract(fact)

    # Add more methods as needed for specific MeTTa functionality
