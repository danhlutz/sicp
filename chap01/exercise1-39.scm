(define (cont-frac n d k)
  (cont-frac-rec n d k 1))

(define (cont-frac-rec n d k index)
  (let ((n-value (n index))
        (d-value (d index)))
    (if (= k index)
        (/ n-value d-value)
        (/ n-value (+ d-value (cont-frac-rec n d k (+ index 1)))))))

(define (tan-cf x k)
  (define (get-numerator index)
    (if (= index 1)
        x
        (- (expt x index))))
  (cont-frac get-numerator
             (lambda (index) (- (* 2 index) 1))
             k))
