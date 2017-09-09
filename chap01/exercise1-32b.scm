(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (cube n) (* n n n))

(define (inc n) (+ n 1))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity n) n)

(define (factorial n)
  (product identity 1 inc n))
