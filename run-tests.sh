#!/bin/bash

# This script runs the ERT tests for the org-lectures package.
# It assumes the .el files are already up-to-date.
# It does the following:
# 1. Runs all tests defined in files ending with -tests.el inside the tests/ directory.
# 2. Exits with a non-zero status code if any test fails.

emacs --batch \
      -L . \
      -l ert \
      --eval '(progn
                (load (expand-file-name "tests/test-helper.el"))
                (dolist (file (file-expand-wildcards "tests/*-tests.el"))
                  (load file)))' \
      --eval '(kill-emacs (if (ert-run-tests-batch t) 0 1))'
