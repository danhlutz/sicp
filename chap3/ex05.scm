(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((area (* (- y2 y1)
                 (- x2 x1))))
    (* area (monte-carlo trials (in-p-test P x1 x2 y1 y2)))))

(define (in-p-test P x1 x2 y1 y2)
  (lambda ()
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y))))

(define (in-unit-circle? x y)
  (< (+ (square x) (square y)) 1))

(newline)
(display "estimating pi by using monte carlo integration of the unit circle")
(newline)
(display "100 trials: ")
(display (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 100))
(newline)
(display "10000 trials: ")
(display (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 10000))
(newline)
(display "1000000 trials: ")
(display (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 1000000))


