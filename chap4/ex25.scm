(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; evaluating factorial of 5 in regular scheme
;; causes a maximum recursion depth error
;; that's because scheme tries to evaluate each 
;; argument to unless, and therefore it keeps recursing without hitting
;; a base case.
