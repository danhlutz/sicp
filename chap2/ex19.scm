(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount 
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(define (no-more? coin-values)
  (if (= (length coin-values) 0)
      true
      false))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 50 25 10 5 1))

(define backwards-coins (list 1 5 10 25 50))

(define mixed-up (list 1 10 5 50 25))

(newline)
(display "Testing with coins biggest to smallest: ")
(display (cc 100 us-coins))
(newline)
(display "Testing with coins smallest to biggest: ")
(display (cc 100 backwards-coins))
(newline)
(display "Testing with mixed-up coins: ")
(display (cc 100 mixed-up))

; analysis
; the new version of cc gives the same answer no matter what order the 
; coins are presented in
; that's because this time we are not representing the coins as 
; arbitrary, ordered integers, but instead as their values instead. 
; we can pass those values directly to the recursive definition 
