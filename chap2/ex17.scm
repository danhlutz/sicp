(define (last-pair item)
  (if (= (length item) 1)
      item
      (last-pair (cdr item))))
