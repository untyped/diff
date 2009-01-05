#lang scheme/base

(require "base.ss")

; Structs ----------------------------------------

; (struct integer integer (vectorof vector))
(define-struct matrix (x y data) #:transparent)

; Procedures -------------------------------------

; integer integer -> (vectorof vector)
(define (create-matrix x y [v 0])  
  (make-matrix x y
               (let ([vector-x (make-vector x)])
                 (for ([x-th (in-range x)])
                   (vector-set! vector-x x-th (make-vector y v)))
                 vector-x)))

; Provides ---------------------------------------

(provide (struct-out matrix))

(provide/contract
 [create-matrix (->* ((and/c natural-number/c (>/c 0))
                      (and/c natural-number/c (>/c 0)))
                     (any/c)
                     matrix?)])