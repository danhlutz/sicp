(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (average-damp (lambda (guess) (/ x guess))))
   1.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1)
    (< (abs (- v1 (improve-guess v1))) tolerance))
  (define (improve-guess guess)
    (f guess))
  ((iterative-improve close-enough?
                      improve-guess)
   first-guess))

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
    
