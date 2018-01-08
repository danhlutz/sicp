(load "constraint.scm")

(define (c+ a b)
  (let ((z (make-connector)))
    (adder a b z)
    z))

(define (c* a b)
  (let ((z (make-connector)))
    (multiplier a b z)
    z))

(define (cv val)
  (let ((z (make-connector)))
    (constant val z)
    z))

(define (c- a b)
  (c+ a (c* (cv -1) b)))

(define (c/ a b)
  (let ((z (make-connector)))
    (divider a b z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(probe "C" C)

(define f (celsius-fahrenheit-converter C))
(probe "F" F)
