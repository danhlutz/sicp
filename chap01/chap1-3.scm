(define (cube n)
  (* n n n))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a 
       (lambda (x) (+ x 4))
       b))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (identity x) x)

(define (integral f a b dx)
  (* (sum f 
          (+ a (/ dx 2.0)) 
          (lambda (x) (+ x dx)) 
          b)
     dx))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
