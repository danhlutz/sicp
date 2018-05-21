(load "compiler.scm")

(define alt-factorial
  '(begin
     (define (factorial-alt n)
       (if (= n 1)
           1
           (* n (factorial-alt (- n 1)))))
     (factorial-alt 6)))
