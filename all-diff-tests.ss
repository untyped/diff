#lang scheme/base

(require "test-base.ss"
         "lcs-test.ss"
         "matrix-test.ss")

; Tests -------------------------------------------

(define all-diff-tests
  (test-suite "diff"
    matrix-tests
    lcs-tests))

; Provide statements -----------------------------

(provide all-diff-tests)