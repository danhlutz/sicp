(define (filtered-accumulate combiner null-value filter? term a next b)
  (define (iter a result)
    (cond ((> a b) 
           result)
          ((filter? a)
           (iter (next a) 
                 (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum-primes-squares a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (relative-prime-product n)
  (define (relative-prime? a)
    (= (gcd n a) 1))
  (filtered-accumulate * 1 relative-prime? identity 1 inc (- n 1)))

(define (inc n) (+ n 1))

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

(define (identity n) n)
