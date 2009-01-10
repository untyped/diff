#lang scheme/base

(require "all-diff-tests.ss"
         "test-base.ss")

(parameterize ([print-struct #t])
  (run-tests all-diff-tests))
