(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define (make-even n)
    (if (even? n)
        n
        (+ n 1)))
  (define h
    (/ (- b a) (make-even n)))
  (define (add-h x)
    (+ x h))
  (define (add-2h x)
    (+ x (* 2 h)))
  (* (/ h 3.0)
     (+ (sum f a add-h b)
        (sum f (+ a h) add-h (- b h))
        (* 2
           (sum f (+ a h) add-2h (- b h))))))

(define (cube x) (* x x x))

(newline)
(display "Integral estimate 100 for cube: ")
(display (integral cube 0 1 0.01))
(newline)
(display "Simpson estimate for 100 for cube: ")
(display (simpson cube 0 1 100))
(newline)
(display "Integral estimate for 1000: ")
(display (integral cube 0 1 0.001))
(newline)
(display "Simpson estimate for 1000: ")
(display (simpson cube 0 1 1000))
(newline)
(display "Integral 10,000: ")
(display (integral cube 0 1 0.0001))
(newline)
(display "Simpson 10,000: ")
(display (simpson cube 0 1 10000))