#!/bin/bash
# Bootstrap chipsalliance/sv-tests at $HOME/sv-tests and link our
# three runner files in. Idempotent — re-running just refreshes the
# symlinks. Doesn't touch the sv-tests tree beyond `tools/runners/`.

set -e
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
sv_tests=${SV_TESTS_DIR:-$HOME/sv-tests}

if [ ! -d "$sv_tests" ]; then
    echo "==> cloning sv-tests into $sv_tests"
    git clone --depth 1 https://github.com/chipsalliance/sv-tests.git "$sv_tests"
else
    echo "==> sv-tests already present at $sv_tests"
fi

if [ -f "$sv_tests/conf/requirements.txt" ]; then
    echo "==> installing Python deps (skip if already satisfied)"
    pip3 install --user --quiet -r "$sv_tests/conf/requirements.txt" \
        || echo "(pip3 install hit issues — continuing; sv-tests may still partially work)"
fi

# Symlink each Decompiler_*.py runner from our repo into sv-tests.
echo "==> installing runner symlinks into $sv_tests/tools/runners/"
for r in "$here"/runners/Decompiler_*.py; do
    [ -e "$r" ] || continue
    name=$(basename "$r")
    target="$sv_tests/tools/runners/$name"
    rm -f "$target"
    ln -s "$r" "$target"
    echo "  - $name → $r"
done

# Same for the wrapper.
mkdir -p "$sv_tests/tools/wrappers"
for w in "$here"/wrappers/decompiler_*.sh; do
    [ -e "$w" ] || continue
    name=$(basename "$w")
    target="$sv_tests/tools/wrappers/$name"
    rm -f "$target"
    ln -s "$w" "$target"
    echo "  - $name → $w"
done

echo
echo "==> verifying our runners are discoverable"
cd "$sv_tests"
ls tools/runners/Decompiler_*.py 2>&1 | sed 's/^/  /'

# Optional sv-parser oracle (dalance/sv-parser).  sv-tests already
# ships a tools/runners/sv_parser.py runner pointing at the
# `parse_sv` binary, but expects it on PATH or under
# out/runners/bin/.  If the user has a sibling sv-parser checkout
# with `parse_sv` built (`cargo build --example parse_sv --release`),
# symlink it into the runner-bin directory so the runner finds it.
sv_parser_dir=${SV_PARSER_DIR:-$HOME/sv-parser}
sv_parser_exe="$sv_parser_dir/target/release/examples/parse_sv"
if [ -x "$sv_parser_exe" ]; then
    mkdir -p "$sv_tests/out/runners/bin"
    rm -f "$sv_tests/out/runners/bin/parse_sv"
    ln -s "$sv_parser_exe" "$sv_tests/out/runners/bin/parse_sv"
    echo "==> sv-parser oracle wired: $sv_parser_exe"
else
    echo "==> sv-parser oracle skipped (no $sv_parser_exe — run \`cargo build --example parse_sv --release\` in $sv_parser_dir to enable)"
fi

# Optional sv2v oracle (zachjs/sv2v).  sv-tests ships a
# tools/runners/Sv2v_zachjs.py runner that invokes `zachjs-sv2v`;
# wire a sibling ~/sv2v checkout (built with `make` → bin/sv2v) in
# under that name so the runner picks it up.
sv2v_dir=${SV2V_DIR:-$HOME/sv2v}
sv2v_exe="$sv2v_dir/bin/sv2v"
if [ -x "$sv2v_exe" ]; then
    mkdir -p "$sv_tests/out/runners/bin"
    rm -f "$sv_tests/out/runners/bin/zachjs-sv2v"
    ln -s "$sv2v_exe" "$sv_tests/out/runners/bin/zachjs-sv2v"
    echo "==> sv2v oracle wired:      $sv2v_exe"
else
    echo "==> sv2v oracle skipped (no $sv2v_exe — run \`make\` in $sv2v_dir to enable)"
fi

echo
echo "Install complete. Repo: $repo  |  sv-tests: $sv_tests"
echo "Next: bash $here/run.sh"
