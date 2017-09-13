(define (cont-frac n d k)
  (cont-frac-rec n d k 1))

(define (cont-frac-rec n d k index)
  (let ((n-value (n index))
        (d-value (d index)))
    (if (= k index)
        (/ n-value d-value)
        (/ n-value (+ d-value (cont-frac-rec n d k (+ index 1)))))))

(define (threven? n)
  (= (remainder n 3) 0))

(define (euler-expand k)
  (define (get-denom index)
    (if (threven? (+ index 1))
        (* 2 (/ (+ index 1) 3))
        1))
  (+ 2.0 
     (cont-frac (lambda (i) 1)
                get-denom
                k)))

(newline)
(display "Calculating e using a continuous fraction")
(newline)
(display "k = 5")
(newline)
(display (euler-expand 5))
(newline)
(display "k = 10")
(newline)
(display (euler-expand 10))
(newline)
(display "k = 25")
(newline)
(display (euler-expand 25))
(newline)
(display "k = 100")
(newline)
(display (euler-expand 100))
