(newline)
(display "Calculating 10! ")
(display
  ((lambda (n)
     ((lambda (fact)
        (fact fact n))
      (lambda (ft k)
        (if (= k 1)
            1
            (* k (ft ft (- k 1)))))))
   10)
  )

(newline)
(display "Calculating the fib of 10: ")
(display
  ((lambda (n)
     ((lambda (fib)
        (fib fib n))
      (lambda (ft k)
        (cond ((= k 0) 0)
              ((= k 1) 1)
              (else (+ (ft ft (- k 1))
                       (ft ft (- k 2))))))))
   20))


