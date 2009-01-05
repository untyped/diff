#lang scheme/base

(require "base.ss"
         "matrix-internal.ss")

; (vectorof vector) integer integer -> any
(define (matrix-ref matrix x y)
  (matrix-index-check matrix x y)  
  (vector-ref (vector-ref (matrix-data matrix) x) y))

; (vectorof vector) integer integer any -> void
(define (matrix-set! matrix x y v)
  (matrix-index-check matrix x y)
  (vector-set! (vector-ref (matrix-data matrix) x) y v))

; (vectorof vector) integer integer -> void
(define (matrix-index-check matrix x y)
  ; check X dimension
  (let ([x-len (matrix-x matrix)])
    (when (>= x x-len)
      (raise-exn exn:fail:contract 
        (format "x-index specified [~a] is too large. Expected 0 <= x < ~a" x x-len))))
  
  ; check Y dimension
  (let* ([y-len (matrix-y matrix)])
    (when (>= y y-len)
      (raise-exn exn:fail:contract
        (format "y-index specified [~a] is too large. Expected 0 <= y < ~a" y y-len))))
  ; returns if all ok
  (void))

; (vectorof vector) -> integer
(define (matrix-x-length matrix)
  (matrix-x matrix))

; (vectorof vector) -> integer
(define (matrix-y-length matrix)
  (matrix-y matrix))

; Provides ---------------------------------------

(provide (except-out (struct-out matrix) make-matrix)
         (rename-out [create-matrix make-matrix]))

(provide/contract
 [matrix-ref      (-> matrix? natural-number/c natural-number/c any/c)]
 [matrix-set!     (-> matrix? natural-number/c natural-number/c any/c void?)]
 [matrix-x-length (-> matrix? (and/c natural-number/c (>/c 0)))]
 [matrix-y-length (-> matrix? (and/c natural-number/c (>/c 0)))])