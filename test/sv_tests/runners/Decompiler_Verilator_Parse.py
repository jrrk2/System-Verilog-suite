# SPDX-License-Identifier: ISC
"""sv-tests runner for the System-Verilog-suite Verilator-JSON
frontend.

Pipeline (in tools/wrappers/decompiler_verilator_parse.sh):
  verilator --json-only ...  →  V<top>.tree.json
  test_verilator_behavioral.exe <json>  →  rc=0 if ≥ 1 module emerged.

Passes when both stages succeed; fails if verilator rejects the SV or
our converter throws / produces no modules.
"""

import os
from BaseRunner import BaseRunner

_THIS = os.path.dirname(os.path.realpath(__file__))
_REPO = os.path.realpath(os.path.join(_THIS, '..', '..', '..'))
_EXE = os.path.join(_REPO, '_build', 'default', 'test_verilator_behavioral.exe')
_WRAPPER = os.path.join(
    _REPO, 'test', 'sv_tests', 'wrappers', 'decompiler_verilator_parse.sh')


class Decompiler_Verilator_Parse(BaseRunner):
    def __init__(self):
        super().__init__(
            "decompiler_verilator_parse",
            executable=_EXE,
            supported_features={'parsing', 'elaboration'})
        self.url = "https://github.com/jonathankimmitt/System-Verilog-suite"

    def can_run(self):
        # Need our exe AND verilator on PATH AND the wrapper.
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
