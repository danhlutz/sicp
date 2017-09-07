(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

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

;(define (search-for-primes start-number counter)
;  (cond ((even? start-number) 
;         (search-for-primes (+ start-number 1) counter))
;        ((prime? 

(define (make-odd n)
  (if (even? n)
      (+ n 1)
      n))

(define (search-for-primes-iter n primes-to-find)
  (cond ((= primes-to-find 0) 
         (newline)
         (display "done"))
        ((prime? n) 
         (timed-prime-test n)
         (search-for-primes-iter (+ n 2) (- primes-to-find 1)))
        (else (search-for-primes-iter (+ n 2) primes-to-find))))

(define (search-for-primes n primes-to-find)
  (search-for-primes-iter (make-odd n) primes-to-find))

; (search-for-primes 1000 3)
; (search-for-primes 10000 3)
; (search-for-primes 100000 3)
; (search-for-primes 1000000 3)
; (search-for-primes 1000000000 3)
(search-for-primes 1000000000000 3)
(search-for-primes 1000000000000000 3)

; output
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

; comparison of performance as size increases  
; size of primes
; (/ 1000000000000037 1000000000039) = 999.999999961037
; times 
; (/ 43001 1409) = 30.518807665010645
; (sqrt 999.9999) = 31.622776601067734

; the amount of time required by algorithm has gone up very closely to 
; sqrt n -- which was expected

