#lang scheme/base

(require "lcs.ss"
         "matrix.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define lcs-tests
  (test-suite "Tests for lcs.ss"    
    
    (test-case "make-lcs-matrix"
      (let ([matrix (make-lcs-matrix (list->vector (string->list "7777777"))
                                     (list->vector (string->list "88888888")))]) 
        (check = (matrix-x matrix) 8)
        (check = (matrix-y matrix) 9)))
    
    (test-equal? "lcs-length"
      (lcs-length (list->vector (string->list "XMJYAUZ"))
                  (list->vector (string->list "MZJAWXU")))
      4)
    
    (test-equal? "lcs-backtrace"
      (lcs-backtrace (list->vector (string->list "XMJYAUZ"))
                     (list->vector (string->list "MZJAWXU")))
      (string->list "MJAU"))
    
    (test-equal? "lcs-backtrace-all"
      (lcs-backtrace-all (list->vector (string->list "XMJYAUZ"))
                         (list->vector (string->list "MZJAWXU")))
      (list (string->list "MJAU")))
    
    (test-equal? "lcs-diff"
      (lcs-diff (list->vector (string->list "abcdfghjqz"))
                (list->vector (string->list "abcdefgijkrxyz")))
      (list (make-diff 1 diff-op:noop #\a)
            (make-diff 1 diff-op:noop #\b)
            (make-diff 1 diff-op:noop #\c)
            (make-diff 1 diff-op:noop #\d)
            (make-diff 1 diff-op:insert #\e)
            (make-diff 1 diff-op:noop #\f)
            (make-diff 1 diff-op:noop #\g)
            (make-diff 1 diff-op:delete #\h)
            (make-diff 1 diff-op:insert #\i)
            (make-diff 1 diff-op:noop #\j)
            (make-diff 1 diff-op:delete #\q)
            (make-diff 1 diff-op:insert #\k)
            (make-diff 1 diff-op:insert #\r)
            (make-diff 1 diff-op:insert #\x)
            (make-diff 1 diff-op:insert #\y)
            (make-diff 1 diff-op:noop #\z)))))

; Provides ---------------------------------------

(provide lcs-tests)