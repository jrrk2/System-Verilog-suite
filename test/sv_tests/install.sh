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
echo
echo "Install complete. Repo: $repo  |  sv-tests: $sv_tests"
echo "Next: bash $here/run.sh"
