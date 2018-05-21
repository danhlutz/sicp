(load "compiler.scm")

(define fact-iter
  '(begin
     (define (factorial n)
       (define (iter product counter)
         (if (> counter n)
             product
             (iter (* product counter)
                   (+ counter 1))))
       (iter 1 1))
     (factorial 6)))
