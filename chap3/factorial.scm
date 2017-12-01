(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (newline)
  (display "product: ")
  (display product)
  (display " | counter: ")
  (display counter)
  (if (> counter max-count)
      product
      (fact-iter (* product counter)
                 (+ counter 1)
                 max-count)))
