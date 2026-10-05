#!/bin/sh
# Usage: tests/test.sh
# Runs under Roswell's SBCL when ros is installed, as the launcher does, else
# the sbcl on PATH. MIAO_TEST_LISP=ros|sbcl chooses.
set -e

QL="${QUICKLISP_SETUP:-$HOME/quicklisp/setup.lisp}"
RUN="(progn (ql:quickload :miao/tests :silent t) (asdf:test-system :miao))"

if [ -z "$MIAO_TEST_LISP" ]; then
    if command -v ros >/dev/null 2>&1; then MIAO_TEST_LISP=ros; else MIAO_TEST_LISP=sbcl; fi
fi

case "$MIAO_TEST_LISP" in
    ros)  exec ros -Q run -- --non-interactive --no-userinit --load "$QL" --eval "$RUN" ;;
    sbcl) exec sbcl --non-interactive --no-userinit --load "$QL" --eval "$RUN" ;;
    *)    echo "tests/test.sh: MIAO_TEST_LISP must be ros or sbcl" >&2; exit 2 ;;
esac
