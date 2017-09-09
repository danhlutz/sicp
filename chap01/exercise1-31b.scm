(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (identity n) n)

(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))


