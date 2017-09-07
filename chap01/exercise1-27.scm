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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test-specific n a)
  (= (expmod a n n) a))

(define (fermat-test-iter n test-number)
  (cond ((= test-number 0)
          true)
        ((fermat-test-specific n test-number)
         (fermat-test-iter n (- test-number 1)))
        (else false)))

(define (fermat-test-range n)
  (newline)
  (display "Testing with Fermat ")
  (display n)
  (display ": ")
  (display (fermat-test-iter n (- n 1))))

(define (prime-test n)
  (newline)
  (display "Testing with brute force ")
  (display n)
  (display ": ")
  (display (prime? n)))

(define (test-both n)
  (fermat-test-range n)
  (prime-test n))


(test-both 561)
(test-both 1105)
(test-both 1729)
(test-both 2465)
(test-both 2821)
(test-both 6601)
