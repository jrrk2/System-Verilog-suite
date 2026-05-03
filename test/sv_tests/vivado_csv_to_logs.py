#!/usr/bin/env python3
"""Translate vivado_baseline.csv into per-test sv-tests log files
under out/logs/Vivado_Synth_RTL/. Each log carries the YAML-style
header sv-report parses (name, tags, rc, tool_success, runner, …) so
the next `make report` adds a Vivado column to the HTML grid.

Run:
    python3 test/sv_tests/vivado_csv_to_logs.py
        [--sv-tests ~/sv-tests]
        [--csv     <out/vivado_baseline.csv>]
        [--runner-name Vivado_Synth_RTL]
"""
import argparse
import csv
import os
import re
import sys
import datetime

# Tests that aren't in the CSV (e.g. chapter-18 randomization, or
# Vivado-crash placeholders) get an "untested" log marker so the
# dashboard knows the runner skipped them rather than silently
# pretending they passed.
UNTESTED_REASON = "skipped (Vivado segfault / non-synth chapter)"


def parse_test_metadata(sv_path):
    """Pull the YAML-ish `:key: value` block from a test's comment
    header. sv-tests stores the canonical metadata there. Returns a
    dict (missing keys → empty string)."""
    meta = {
        'name': '', 'description': '', 'tags': '', 'should_fail': '0',
        'should_fail_because': '', 'defines': '', 'incdirs': '',
        'top_module': '', 'timeout': '30',
        'type': 'parsing elaboration',
        'compatible-runners': 'all', 'unsynthesizable': '0',
        'results_group': '',
    }
    try:
        with open(sv_path, 'r', errors='replace') as f:
            text = f.read()
    except OSError:
        return meta
    # Headers are inside `/* ... */` near the top.
    m = re.search(r"/\*(.*?)\*/", text, re.DOTALL)
    block = m.group(1) if m else text[:2000]
    for line in block.splitlines():
        m = re.match(r"\s*:([\w-]+):\s*(.*?)\s*$", line)
        if m:
            k, v = m.group(1), m.group(2)
            meta[k] = v
    return meta


def write_log(out_dir, runner_name, test_name, meta, csv_row, sv_path):
    """Emit one log file under out_dir/runner_name/<test_name>.log."""
    log_path = os.path.join(out_dir, runner_name, test_name + ".sv.log")
    os.makedirs(os.path.dirname(log_path), exist_ok=True)
    if csv_row is None:
        rc = "1"
        tool_success = "0"
        ms = "0"
        err = UNTESTED_REASON
        result = "SKIP"
    else:
        result = csv_row['result']
        rc = "0" if result == "PASS" else "1"
        tool_success = "1" if result == "PASS" else "0"
        ms = csv_row['ms']
        err = csv_row['error'].strip('"')
    fields = [
        ('name', meta.get('name', '') or test_name.split('/')[-1]),
        ('description', meta.get('description', '')),
        ('tags', meta.get('tags', '')),
        ('files', sv_path),
        ('incdirs', os.path.dirname(sv_path)),
        ('top_module', meta.get('top_module', '')),
        ('timeout', meta.get('timeout', '30')),
        ('type', meta.get('type', 'parsing elaboration')),
        ('should_fail', meta.get('should_fail', '0')),
        ('should_fail_because', meta.get('should_fail_because', '')),
        ('defines', meta.get('defines', '')),
        ('compatible-runners', meta.get('compatible-runners', 'all')),
        ('unsynthesizable', meta.get('unsynthesizable', '0')),
        ('results_group', meta.get('results_group', '')),
        ('mode', 'elaboration'),
        ('rc', rc),
        ('tool_success', tool_success),
        ('runner', runner_name.lower()),
        ('runner_url',
         'https://www.xilinx.com/products/design-tools/vivado.html'),
        ('time_elapsed', str(int(ms) / 1000.0)),
        ('user_time', '0'),
        ('system_time', '0'),
        ('ram_usage', '0'),
        ('date_completed',
         datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")),
    ]
    with open(log_path, 'w') as f:
        for k, v in fields:
            f.write(f"{k}: {v}\n")
        f.write("\n")
        f.write(f"vivado_batch_synth.tcl  read_verilog -sv  synth_design "
                f"-rtl -top {meta.get('top_module') or 'top'} -part xc7a35tcpg236-1\n")
        if err:
            f.write(f"\n{result}: {err}\n")
        else:
            f.write(f"\n{result}\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sv-tests", default=os.path.expanduser("~/sv-tests"))
    ap.add_argument("--csv", default=None)
    ap.add_argument("--runner-name", default="Vivado_Synth_RTL")
    args = ap.parse_args()

    csv_path = args.csv or os.path.join(args.sv_tests, "out", "vivado_baseline.csv")
    if not os.path.isfile(csv_path):
        sys.exit(f"no CSV at {csv_path} — run vivado_baseline.sh first")

    out_dir = os.path.join(args.sv_tests, "out", "logs")
    tests_root = os.path.join(args.sv_tests, "tests")

    # Index the CSV by test_name.
    by_name = {}
    with open(csv_path) as f:
        for row in csv.DictReader(f):
            by_name[row['test']] = row

    written = 0
    skipped = 0
    for sv_path in sorted(_walk_sv(tests_root)):
        rel = os.path.relpath(sv_path, tests_root)[:-3]   # strip .sv
        meta = parse_test_metadata(sv_path)
        # Skip tests our oracle filter excludes — keeps dashboard
        # consistent with the runners' get_mode behaviour.
        if meta.get('unsynthesizable') == '1':
            continue
        tags = meta.get('tags', '').split()
        if any(t.startswith('uvm') or t == 'testbench' for t in tags):
            continue
        row = by_name.get(rel)
        write_log(out_dir, args.runner_name, rel, meta, row, sv_path)
        if row is None:
            skipped += 1
        else:
            written += 1

    # Version stamp the dashboard reads.
    vdir = os.path.join(out_dir, args.runner_name)
    with open(os.path.join(vdir, "version"), 'w') as f:
        f.write("Vivado v2020.1 (synth_design -rtl)\n")
    with open(os.path.join(vdir, "url"), 'w') as f:
        f.write("https://www.xilinx.com/products/design-tools/vivado.html\n")

    print(f"wrote {written} result logs + {skipped} skip-marker logs to "
          f"{vdir}")


def _walk_sv(root):
    for dp, _, fs in os.walk(root):
        for fn in fs:
            if fn.endswith('.sv'):
                yield os.path.join(dp, fn)


if __name__ == "__main__":
    main()
