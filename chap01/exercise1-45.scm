(define (average a b)
  (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define tolerance 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define dx 0.00001)

(define (approx-log-2 number)
  (define (iter number guess)
    (let ((two-power (expt 2 guess)))
      (cond ((= two-power number) guess)
            ((> two-power number) guess)
            (else (iter number (+ guess 1))))))
  (iter number 1))

(define (root x n)
  (let ((average-damp-n (repeated average-damp (approx-log-2 n))))
    (fixed-point (average-damp-n (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))
