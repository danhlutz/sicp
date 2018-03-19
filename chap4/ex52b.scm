(load "ex52.scm")

(interpret
  '(define (even? x)
     (= (remainder x 2) 0)))

(interpret
  '(define (find-even items)
     (let ((x (an-element-of items)))
       (require (even? x))
       x)))
