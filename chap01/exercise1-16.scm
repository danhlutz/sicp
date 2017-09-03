(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (fast-expt-iter (* b b) (/ n 2) a)
          (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))
