(load "constraint.scm")

(define (averager a b avg)
  (let ((sum (make-connector))
        (div (make-connector)))
    (adder a b sum)
    (multiplier sum div avg)
    (constant 0.5 div)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define AVG (make-connector))

(probe "A value" A)
(probe "B value" B)
(probe "Average" AVG)

(averager A B AVG)
