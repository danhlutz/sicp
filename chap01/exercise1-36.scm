(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "Guess: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2.0))

(define (xx-no-damp)
  (fixed-point (lambda (x) (/ 1000 (log x)))
               2.0))

(define (xx-damp)
  (fixed-point (lambda (x) (average x (/ 1000 (log x))))
               2.0))

(newline)
(display "Computing fixed-point of x^x")
(newline)
(display "Without using average damping")
(newline)
(display (xx-no-damp))
(newline)
(display "With using average damping")
(newline)
(display (xx-damp))

; With no damping it takes 13 steps. 20 steps with damping
