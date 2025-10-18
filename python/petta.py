import os
import uuid
import threading
import janus_swi as janus

CONSULTED = False
CONSULT_LOCK = threading.Lock()

class PeTTa:
    def __init__(self, verbose=False, metta_src_path=None):
        global CONSULTED
        self.cmd = "process_metta_string" if verbose else "process_metta_string_silent"

        if not CONSULTED:
            with CONSULT_LOCK:
                if not CONSULTED:
                    if metta_src_path is None:
                        # Assume the src directory is in the same directory as this file
                        metta_src_path = os.path.join(os.path.dirname(__file__))
                    main_path = os.path.join(metta_src_path, '..', 'src' , 'main.pl')
                    helper_path = os.path.join(metta_src_path, 'helper.pl')
                    janus.consult(main_path)
                    janus.consult(helper_path)
                    CONSULTED = True

    def load_metta_file(self, file_path):
        """Compile a MeTTa file to Prolog and return the results of the run."""
        with open(file_path, 'r') as f:
            metta_code = f.read()
        return self.process_metta_string(metta_code)

    def process_metta_string(self, metta_code, run_arg=None):
        """Compile a string of MeTTa code to Prolog and return the results of the run."""
        if run_arg is None:
            run_arg = str(uuid.uuid4())
        # Use parameterized query to call process_metta_string with the run_arg
        janus.query_once(f"{self.cmd}('{metta_code}', 'A{run_arg}')")
        # Now query for the run results for this specific run_arg
        results = list(janus.query(f"crun('A{run_arg}',R)"))
        return [result['R'] for result in results]

    def run(self):
        """Execute the run(R) predicates and return results."""
        results = list(janus.query("crun(A,R)"))
        return [result['R'] for result in results]
