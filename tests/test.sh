#!/bin/sh
# Usage: tests/test.sh
set -e

QL="${QUICKLISP_SETUP:-$HOME/quicklisp/setup.lisp}"
RUN="(progn (ql:quickload :miao/tests :silent t) (asdf:test-system :miao))"

exec sbcl --non-interactive --no-userinit --load "$QL" --eval "$RUN"
