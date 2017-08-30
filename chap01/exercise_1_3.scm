(define (square a) (* a a))


(define (sum_of_squares a b)
  (+ (square a) (square b)))


(define (sum_two_biggest a b c)
  (cond ((and (< c a) (< c b)) (sum_of_squares a b))
        ((and (< b a) (< b c)) (sum_of_squares a c))
        (else (sum_of_squares b c))))

