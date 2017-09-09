(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (inc n) (+ n 1))
  (define (identity n) n)
  (product identity 1 inc n))

(define (pi-product max-num)
  (define (square n) (* n n))
  (define (inc2 n) (+ n 2))
  (* 8.0 
     max-num
     (/ (product square 4 inc2 (- max-num 2))
        (product square 3 inc2 (- max-num 1)))))
