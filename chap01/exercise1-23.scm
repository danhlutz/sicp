; output with original prime? check
; 1009 *** 0
; 1013 *** 0
; 1019 *** 0
; done
; 10007 *** 0
; 10009 *** 0
; 10037 *** 0
; done
; 100003 *** 1
; 100019 *** 0
; 100043 *** 1
; done
; 1000003 *** 2
; 1000033 *** 1
; 1000037 *** 1
; done
; 1000000007 *** 45
; 1000000009 *** 49
; 1000000021 *** 48
; done
; 1000000000039 *** 1409
; 1000000000061 *** 1358
; 1000000000063 *** 1367
; done
; 1000000000000037 *** 43001
; 1000000000000091 *** 42579
; 1000000000000159 *** 42472

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
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

; analysis -- picking the last number, the improvement increased the speed
; by a factor of 1.65 -- close to, but not at, the doubling of improvement
; I expected
