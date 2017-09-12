(define (cont-frac n d k)
  (cont-frac-rec n d k 1))


(define (cont-frac-rec n d k index)
  (let ((n-value (n index))
        (d-value (d index)))
    (if (= k index)
        (/ n-value d-value)
        (/ n-value (+ d-value (cont-frac-rec n d k (+ index 1)))))))
