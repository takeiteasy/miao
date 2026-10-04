#!/bin/sh
# Usage: tests/test.sh
# Runs under Roswell's SBCL when ros is installed, as the launcher does, else
# the sbcl on PATH. MIAO_TEST_LISP=ros|sbcl|ecl chooses.
set -e

QL="${QUICKLISP_SETUP:-$HOME/quicklisp/setup.lisp}"
RUN="(progn (ql:quickload :miao/tests :silent t) (asdf:test-system :miao))"

if [ -z "$MIAO_TEST_LISP" ]; then
    if command -v ros >/dev/null 2>&1; then MIAO_TEST_LISP=ros; else MIAO_TEST_LISP=sbcl; fi
fi

ECL_RUN="(handler-case (progn (ql:quickload :miao/tests :silent t) (ext:quit (if (asdf:test-system :miao) 0 1))) (error (e) (format t \"~a~%\" e) (ext:quit 1)))"

case "$MIAO_TEST_LISP" in
    ros)  exec ros -Q run -- --non-interactive --no-userinit --load "$QL" --eval "$RUN" ;;
    sbcl) exec sbcl --non-interactive --no-userinit --load "$QL" --eval "$RUN" ;;
    ecl)  # ECL can die mid-suite with status 0, so a run without fiveam's totals fails.
          OUT="$(mktemp)"
          status=0
          ecl --norc --nodebug --load "$QL" --eval "$ECL_RUN" >"$OUT" 2>&1 || status=$?
          cat "$OUT"
          grep -q "Did [0-9]* checks" "$OUT" || status=1
          trash "$OUT" 2>/dev/null || :
          exit "$status" ;;
    *)    echo "tests/test.sh: MIAO_TEST_LISP must be ros, sbcl or ecl" >&2; exit 2 ;;
esac
