(define (reverse-it item)
  (define (iter item result)
    (if (= (length item) 0)
        result
        (iter (cdr item) (cons (car item) result))))
  (iter item (list)))
