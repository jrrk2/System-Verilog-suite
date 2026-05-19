# SPDX-License-Identifier: ISC
"""sv-tests runner for the System-Verilog-suite Verible→BIR
frontend.

Passes when `test_verible_to_bir.exe <top> <flat.sv>` exits 0
(meaning the Verible parser tree converted to ≥ 1 BIR module
without throwing).

Files-with-`include` or `defines` are pre-flattened by
`tools/wrappers/decompiler_flatten.sh` (which wraps verilator -E),
so this runner does not need to know about preprocessing semantics.
"""

import os
from BaseRunner import BaseRunner

# Resolve the symlink in tools/runners/ back to its real path so we
# can find the sibling repo containing _build/ and the wrapper.
_THIS = os.path.dirname(os.path.realpath(__file__))
_REPO = os.path.realpath(os.path.join(_THIS, '..', '..', '..'))
_EXE = os.path.join(_REPO, '_build', 'default', 'test_verible_to_bir.exe')
_WRAPPER = os.path.join(
    _REPO, 'test', 'sv_tests', 'wrappers', 'decompiler_flatten.sh')


class Decompiler_Verible_Parse(BaseRunner):
    def __init__(self):
        super().__init__(
            "decompiler_verible_parse",
            executable=_EXE,
            supported_features={'parsing', 'elaboration'})
        self.url = "https://github.com/jonathankimmitt/System-Verilog-suite"

    def can_run(self):
        return os.access(_EXE, os.X_OK) and os.access(_WRAPPER, os.X_OK)

    def get_mode(self, params):
        # Skip non-synthesisable tests (UVM, assertions, coverage,
        # display/timescale checks, etc.). Our converters target the
        # synthesisable subset, so testbench-only constructs aren't
        # in scope.
        if params.get('unsynthesizable', '0') == '1':
            return None
        tags = params.get('tags', '').split()
        if any(t.startswith('uvm') or t == 'testbench' for t in tags):
            return None
        return super().get_mode(params)

    def get_version_cmd(self):
        # The exe doesn't have --version; report the binary's mtime as
        # a stand-in so historical reports show drift after a rebuild.
        return ['stat', '-c', '%y', _EXE]

    def prepare_run_cb(self, tmp_dir, params):
        top = (params.get('top_module') or '').strip()
        if not top:
            # sv-tests' usual idiom is `module top();`. Try that
            # first; if the file declares something else our miter
            # will report "no module 'top'" and fail cleanly.
            top = 'top'

        incdirs = ' '.join('-I' + d for d in params.get('incdirs', []))
        defines = ' '.join('+define+' + d for d in params.get('defines', []))

        self.cmd = [
            _WRAPPER, _EXE, top, tmp_dir, incdirs, defines,
        ] + list(params['files'])
