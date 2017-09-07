; output with best next divisor picker
; 1009 *** 0
; 1013 *** 0
; 1019 *** 0
; 10007 *** 1
; 10009 *** 0
; 10037 *** 0
; 100003 *** 1
; 100019 *** 0
; 100043 *** 0
; 1000003 *** 1
; 1000033 *** 1
; 1000037 *** 1
; 1000000007 *** 27
; 1000000009 *** 29
; 1000000021 *** 32
; 1000000000039 *** 862
; 1000000000061 *** 850
; 1000000000063 *** 841
; 1000000000000037 *** 25622
; 1000000000000091 *** 25678
; 1000000000000159 *** 25631

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)

(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)

(timed-prime-test 1000000000000037)
(timed-prime-test 1000000000000091)
(timed-prime-test 1000000000000159)

; output using fast-prime?
; 1009 *** 4
; 1013 *** 4
; 1019 *** 5
; 10007 *** 4
; 10009 *** 4
; 10037 *** 3
; 100003 *** 4
; 100019 *** 5
; 100043 *** 6
; 1000003 *** 6
; 1000033 *** 5
; 1000037 *** 6
; 1000000007 *** 8
; 1000000009 *** 10
; 1000000021 *** 9
; 1000000000039 *** 13
; 1000000000061 *** 13
; 1000000000063 *** 15
; 1000000000000037 *** 16
; 1000000000000091 *** 15
; 1000000000000159 *** 17

; comparison
; (slow-prime-check? 1000000000000159) takes 25631 ticks
; (fast-prime-check? 1000000000000159) takes 17 ticks
; (log 1000000000000159 in base 10) = 34 --- so this is very similar to log
; growth
