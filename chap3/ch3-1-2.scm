(define (rand)
  (random 100000000000000000000))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(newline)
(display "estimate with 100 trials: ")
(display (estimate-pi 100))
(newline)
(display "estimate with 10000 trials: ")
(display (estimate-pi 10000))
(newline)
(display "estimate with 1000000 trials: ")
(display (estimate-pi 1000000))
