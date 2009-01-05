#lang scheme

(require "matrix.ss"
         "test-base.ss")

; Tests -------------------------------------------

(define matrix-tests
  (test-suite "Tests for matrix.ss"    

    (test-case "Matrix dimension reference"
      (let* ([x-len  5]
             [y-len  3]
             [vec-2d (make-matrix x-len y-len)])
        (check = (matrix-x-length vec-2d) x-len)
        (check = (matrix-y-length vec-2d) y-len)))
    
    (test-case "Matrix construction"      
      (let* ([x-len  5]
             [y-len  3]
             [vec-2d (make-matrix x-len y-len)])
        
        ; check X dimensionality
        (check = (matrix-x vec-2d) x-len)
        
        ; check Y dimensionality
        (for ([sub-vec (in-vector (matrix-data vec-2d))]
              [x-th    (in-naturals)])
          (with-check-info (['x-th x-th])
            (check = (vector-length sub-vec) y-len)))
        
        ; check value initialisation
        (for ([sub-vec (in-vector (matrix-data vec-2d))]
              [x-th    (in-naturals)])
          (for ([elem (in-vector sub-vec)]
                [y-th (in-naturals)])
            (with-check-info (['x-th x-th]
                              ['y-th y-th])
              (check-equal? elem 0))))))
    
    (test-case "Matrix construction with value specified"
      (let* ([x-len  5]
             [y-len  3]
             [val    'test]
             [vec-2d (make-matrix x-len y-len val)])
        
        ; check X dimensionality
        (check = (matrix-x vec-2d) x-len)
        
        ; check Y dimensionality
        (for ([sub-vec (in-vector (matrix-data vec-2d))]
              [x-th    (in-naturals)])
          (with-check-info (['x-th x-th])
            (check = (vector-length sub-vec) y-len)))
        
        ; check value initialisation
        (for ([sub-vec (in-vector (matrix-data vec-2d))]
              [x-th    (in-naturals)])
          (for ([elem (in-vector sub-vec)]
                [y-th (in-naturals)])
            (with-check-info (['x-th x-th]
                              ['y-th y-th])
              (check-equal? elem val))))))
    
    (test-case "Matrix reference and modification" 
      (let* ([x-len  5]
             [y-len  3]
             [vec-2d (make-matrix x-len y-len)])
        (matrix-set! vec-2d 3 2 'test)
        (check-equal? (matrix-ref vec-2d 3 2) 'test)))
    
    (test-case "Matrix initialisation with negative numbers or zero raises an exception"      
      (check-exn exn:fail:contract? (cut make-matrix 0 1))
      (check-exn exn:fail:contract? (cut make-matrix 1 0))
      (check-exn exn:fail:contract? (cut make-matrix -1 1))
      (check-exn exn:fail:contract? (cut make-matrix 1 -1)))))

; Provide statements -----------------------------

(provide matrix-tests)


