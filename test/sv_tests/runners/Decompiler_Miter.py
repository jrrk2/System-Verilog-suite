# SPDX-License-Identifier: ISC
"""sv-tests runner for the System-Verilog-suite
Verilator↔Verible Z3 miter.

Passes only when:
  1. verilator -E flattens the test (so includes/defines work);
  2. test_verilator_vs_verible.exe builds BOTH frontends' BIR;
  3. the Z3 miter proves them equivalent for all inputs.

This is the strictest of the three Decompiler runners — it surfaces
disagreement between the two converters that the per-frontend runners
would miss (each one passing in isolation).
"""

import os
from BaseRunner import BaseRunner

_THIS = os.path.dirname(os.path.realpath(__file__))
_REPO = os.path.realpath(os.path.join(_THIS, '..', '..', '..'))
_EXE = os.path.join(_REPO, '_build', 'default', 'test_verilator_vs_verible.exe')
_WRAPPER = os.path.join(
    _REPO, 'test', 'sv_tests', 'wrappers', 'decompiler_flatten.sh')


class Decompiler_Miter(BaseRunner):
    def __init__(self):
        super().__init__(
            "decompiler_miter",
            executable=_EXE,
            supported_features={'parsing', 'elaboration'})
        self.url = "https://github.com/jonathankimmitt/System-Verilog-suite"

    def can_run(self):
        import shutil
        return (os.access(_EXE, os.X_OK)
                and os.access(_WRAPPER, os.X_OK)
                and shutil.which('verilator') is not None)

    def get_mode(self, params):
        # Skip non-synthesisable tests (UVM, assertions, coverage, …)
        # — out of scope for the decompiler.
        if params.get('unsynthesizable', '0') == '1':
            return None
        tags = params.get('tags', '').split()
        if any(t.startswith('uvm') or t == 'testbench' for t in tags):
            return None
        # Skip should_fail tests: an equivalence miter cannot prove
        # negatives. If both frontends silently accept the illegal SV
        # they will produce identical BIRs and the miter will "prove"
        # them equivalent, which sv-tests then scores as a false-pass
        # because the test expected the tool to reject the input.
        # Either frontend would have to surface the violation as an
        # error for the miter to flag it, and that's a frontend-quality
        # issue, not something equivalence checking can address.
        if params.get('should_fail', '0') == '1':
            return None
        return super().get_mode(params)

    def get_version_cmd(self):
        return ['stat', '-c', '%y', _EXE]

    def prepare_run_cb(self, tmp_dir, params):
        top = (params.get('top_module') or '').strip()
        if not top:
            top = 'top'   # sv-tests convention

        incdirs = ' '.join('-I' + d for d in params.get('incdirs', []))
        defines = ' '.join('+define+' + d for d in params.get('defines', []))

        self.cmd = [
            _WRAPPER, _EXE, top, tmp_dir, incdirs, defines,
        ] + list(params['files'])
