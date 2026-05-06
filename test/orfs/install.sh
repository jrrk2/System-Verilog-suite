#!/bin/bash
# Install / un-install the ORFS Makefile patch that lets us substitute
# our hardcaml synth for yosys+ABC under USE_DECOMP_SYNTH=1.
#
# Usage:
#   test/orfs/install.sh              # apply the patch
#   test/orfs/install.sh --revert     # restore the original Makefile
#   test/orfs/install.sh --check      # report whether the patch is applied
#
# Idempotent: re-applying is a no-op.  Keeps a backup at
# $ORFS/flow/Makefile.orig the first time it patches.

set -euo pipefail

ORFS_DIR="${ORFS_DIR:-$HOME/OpenROAD-flow-scripts}"
MAKEFILE="$ORFS_DIR/flow/Makefile"
BACKUP="$MAKEFILE.orig"
REPO_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
PATCH="$REPO_DIR/test/orfs/decomp_synth.patch"

if [[ ! -f "$MAKEFILE" ]]; then
  echo "ERROR: $MAKEFILE not found.  Set ORFS_DIR if installed elsewhere."
  exit 1
fi

is_applied() {
  grep -q 'USE_DECOMP_SYNTH' "$MAKEFILE" 2>/dev/null
}

case "${1:-}" in
  --check)
    if is_applied; then
      echo "PATCHED: $MAKEFILE has USE_DECOMP_SYNTH support"
    else
      echo "NOT PATCHED: $MAKEFILE is stock"
    fi
    ;;
  --revert)
    if [[ -f "$BACKUP" ]]; then
      cp "$BACKUP" "$MAKEFILE"
      echo "Restored $MAKEFILE from $BACKUP"
    else
      echo "No backup found at $BACKUP — nothing to revert."
      exit 1
    fi
    ;;
  ""|--apply)
    if is_applied; then
      echo "Already patched, skipping."
      exit 0
    fi
    if [[ ! -f "$BACKUP" ]]; then
      cp "$MAKEFILE" "$BACKUP"
      echo "Backed up $MAKEFILE -> $BACKUP"
    fi
    cd "$ORFS_DIR"
    patch -p1 < "$PATCH"
    echo
    echo "Patched.  To use:"
    echo "  cd $ORFS_DIR/flow"
    echo "  USE_DECOMP_SYNTH=1 make DESIGN_CONFIG=designs/nangate45/<design>/config.mk"
    echo
    echo "To revert:"
    echo "  $0 --revert"
    ;;
  *)
    echo "usage: $0 [--apply | --revert | --check]"
    exit 2
    ;;
esac
