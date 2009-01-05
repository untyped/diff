#lang scheme/base

(require (planet untyped/unlib:3/enum)
         (prefix-in set: (planet soegaard/galore:4/set))
         "base.ss"
         "matrix.ss")

; See: http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

; Structure types --------------------------------

; (struct integer diff-op any)
(define-struct diff (index operator item) #:transparent)

(define-enum diff-op
  ([insert "+"]
   [delete "-"]
   [noop   " "])
  #:prefix    diff-op:
  #:predicate diff-op?)

; Procedures -------------------------------------

; vector vector pred -> matrix
(define (make-lcs-matrix a b [same? equal?])
  (let* ([m (vector-length a)]
         [n (vector-length b)]
         [c-array (make-matrix (add1 m) (add1 n))])
    ; C is created and all values set to zero
    
    ; i:{1..m}; j:{1..n}
    (for ([i (in-range 1 (add1 m))])
      (for ([j (in-range 1 (add1 n))])
        (if (same? (vector-ref a (sub1 i)) (vector-ref b (sub1 j)))
            (matrix-set! c-array i j 
                         (add1 (matrix-ref c-array (sub1 i) (sub1 j))))
            (matrix-set! c-array i j 
                         (max (matrix-ref c-array i (sub1 j))
                              (matrix-ref c-array (sub1 i) j))))))    
    c-array))


; matrix -> integer
(define (lcs-length/matrix lcs-matrix)
  (let ([m (sub1 (matrix-x-length lcs-matrix))]
        [n (sub1 (matrix-y-length lcs-matrix))])
    (matrix-ref lcs-matrix m n))) ; C[m,n]

; vector vector [pred] -> integer
(define (lcs-length a b [same? equal?])
  (lcs-length/matrix (make-lcs-matrix a b same?)))


; matrix vector vector pred integer integer list -> list
(define (lcs-backtrace/helper lcs-matrix a b same? i j old-trace)  
  (cond [(or (= i 0) (= j 0)) ; end of sequence, reverse accumulated list
         old-trace]
        [(same? (vector-ref a (sub1 i)) (vector-ref b (sub1 j)))
         (lcs-backtrace/helper lcs-matrix a b same? (sub1 i) (sub1 j)
                               (cons (vector-ref a (sub1 i)) old-trace))]
        [(> (matrix-ref lcs-matrix i (sub1 j))
            (matrix-ref lcs-matrix (sub1 i) j))
         (lcs-backtrace/helper lcs-matrix a b same? i (sub1 j) old-trace)]
        [else
         (lcs-backtrace/helper lcs-matrix a b same? (sub1 i) j old-trace)]))

; matrix vector vector [pred] -> list
; creates an LCS readout for a given LCS matrix
(define (lcs-backtrace/matrix lcs-matrix a b [same? equal?])
  (let ([m (sub1 (matrix-x-length lcs-matrix))]
        [n (sub1 (matrix-y-length lcs-matrix))])
    (lcs-backtrace/helper (make-lcs-matrix a b same?) a b same? m n null)))

; vector vector [pred] -> list
; as lcs-backtrace/matrix, but generates the LCS matrix first
(define (lcs-backtrace a b [same? equal?])
  (lcs-backtrace/matrix (make-lcs-matrix a b same?) a b same?))

; matrix vector vector pred natural natural -> (listof (list))
(define (lcs-backtrace-all/helper lcs-matrix a b same? i j)
  (cond [(or (= i 0) (= j 0))
         (set:make-unordered same? null)] ; set contains only empty list
        [(same? (vector-ref a (sub1 i)) (vector-ref b (sub1 j)))
         (apply set:make-unordered
                equal?
                (for/list ([z (in-list ; each set element
                               (set:elements 
                                (lcs-backtrace-all/helper
                                 lcs-matrix a b same? (sub1 i) (sub1 j))))])
                  (cons (vector-ref a (sub1 i)) z)))]
        [else 
         ; union of possible routes
         (let ([empty-set    (set:make-unordered same?)]
               [val-i-sub1-j (matrix-ref lcs-matrix i (sub1 j))]
               [val-sub1-i-j (matrix-ref lcs-matrix (sub1 i) j)])
           (set:union
            (if (>= val-i-sub1-j val-sub1-i-j)
                (lcs-backtrace-all/helper lcs-matrix a b same? i (sub1 j))
                empty-set)
            (if (>= val-sub1-i-j val-i-sub1-j)
                (lcs-backtrace-all/helper lcs-matrix a b same? (sub1 i) j)
                empty-set)))]))

; matrix vector vector [pred] -> (listof (list))
(define (lcs-backtrace-all/matrix lcs-matrix a b [same? equal?])
  (let ([set-of-lcs (lcs-backtrace-all/helper lcs-matrix a b same? 
                                              (sub1 (matrix-x-length lcs-matrix))
                                              (sub1 (matrix-y-length lcs-matrix)))])
    (for/list ([an-lcs (in-list (set:elements set-of-lcs))])
      (reverse an-lcs))))

; vector vector [pred] -> (listof (list))
(define (lcs-backtrace-all a b [same? equal?])
  (lcs-backtrace-all/matrix (make-lcs-matrix a b same?) a b same?))

; matrix vector vector pred natural natural -> (listof diff)
(define (lcs-diff/helper lcs-matrix a b same? i j)
  (cond [(and (= i 0) (= j 0))
         null]
        [(and (> i 0) (> j 0) (same? (vector-ref a (sub1 i)) (vector-ref b (sub1 j))))
         (cons (make-diff 1 diff-op:noop (vector-ref a (sub1 i)))
               (lcs-diff/helper lcs-matrix a b same? (sub1 i) (sub1 j)))]
        [(and (> j 0) (or (= i 0)
                          (>= (matrix-ref lcs-matrix i (sub1 j))
                              (matrix-ref lcs-matrix (sub1 i) j))))
         (cons (make-diff 1 diff-op:insert (vector-ref b (sub1 j)))
               (lcs-diff/helper lcs-matrix a b same? i (sub1 j)))]
        [(and (> i 0) (or (= j 0)
                          (< (matrix-ref lcs-matrix i (sub1 j))
                             (matrix-ref lcs-matrix (sub1 i) j))))
         (cons (make-diff 1 diff-op:delete (vector-ref a (sub1 i)))
               (lcs-diff/helper lcs-matrix a b same? (sub1 i) j))]))

; matrix vector vector [pred] -> (listof diff)
(define (lcs-diff/matrix lcs-matrix a b [same? equal?])
  (let ([m (sub1 (matrix-x-length lcs-matrix))]
        [n (sub1 (matrix-y-length lcs-matrix))])
    (reverse (lcs-diff/helper lcs-matrix a b same? m n))))

; vector vector pred -> (listof diff)
(define (lcs-diff a b [same? equal?])
  (lcs-diff/matrix (make-lcs-matrix a b same?) a b same?))

; (listof diff)
(define (print-diff diffs)
  (for ([diff-line (in-list diffs)])
    (printf "~a ~a~n" 
            (enum-prettify diff-op (diff-operator diff-line))
            (diff-item diff-line))))

; matrix vector vector -> void
(define (print-lcs-matrix lcs-matrix a b)
  ; natural
  (define cols
    (matrix-x-length lcs-matrix))
  
  ; string
  (define format-string
    (string-append "~a ~a | " (string-join (make-list cols "~a") " ") "~n"))

  ; string string (listof any) -> void
  (define (print-row row-num row-char values)
    (apply printf format-string (list* row-num row-char values)))
  
  ; column headings
  (print-row " " ; no char
             " " ; no char
             (for/list ([n (in-range (matrix-x lcs-matrix))]) n))
  
  (print-row " " ; no char
             " " ; no char
             (list* " " (for/list ([n (in-vector a)]) n)))
  
  (printf "---------------------------------~n")
  
  ; data
  (for ([sub-vec (in-vector (matrix-data lcs-matrix))]
        [row-val (in-list (list* " " (vector->list b)))]
        [row-num (in-naturals)])   
    (print-row row-num row-val (for/list ([n (in-vector sub-vec)]) n))))

; Provides ---------------------------------------

(provide (diff-op-out))

(provide/contract
 [struct diff              ([index    natural-number/c]
                            [operator diff-op?]
                            [item     any/c])]
 [make-lcs-matrix          (->* (vector? vector?) ((-> boolean?)) matrix?)]
 [lcs-length/matrix        (-> matrix? natural-number/c)]
 [lcs-length               (->* (vector? vector?) ((-> boolean?)) natural-number/c)]
 [lcs-backtrace/matrix     (->* (matrix? vector? vector?) ((-> boolean?)) list?)]
 [lcs-backtrace            (->* (vector? vector?) ((-> boolean?)) list?)]
 [lcs-backtrace-all/matrix (->* (matrix? vector? vector?)
                                ((-> boolean?))
                                (listof list?))]
 [lcs-backtrace-all        (->* (vector? vector?) ((-> boolean?)) (listof list?))]
 [lcs-diff/matrix          (->* (matrix? vector? vector?)
                                ((-> boolean?))
                                (listof diff?))]
 [lcs-diff                 (->* (vector? vector?) ((-> any/c any/c boolean?)) (listof diff?))]
 [print-diff               (-> (listof diff?) void?)]
 [print-lcs-matrix         (-> matrix? vector? vector? void?)])