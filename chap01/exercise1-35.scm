; show that The Golden Ratio is the fixed-point procedure that says
; x -> 1 + 1/x

; Phi^2 = Phi + 1 
; Phi = (1 + sqrt 5) / 2

; divide both sides by Phi, yields
; Phi = 1 + 1/ Phi

(define tolerance 0.0000000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(newline)
(display "Calculating using Fixed Point: ")
(display (golden-ratio))
(newline)
(display "Calculating (1 + sqrt 5) / 2: ")
(display (/ (+ 1 (sqrt 5)) 2))
