(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (define (iter a b times)
    (if (= b times)
        a
        (iter (lambda (f) (lambda (x) (f a))) b (lambda (f) (lambda (x) (f times))))))
  (iter a b zero))
