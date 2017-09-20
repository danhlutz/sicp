(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (find-base original num-raised)
  (define (iter a b result)
    (if (= a b)
        result
        (iter (/ a b) b (+ result 1))))
  (iter original num-raised 1))

(define (divides? m n)
  (= (gcd m n) n))

(define (car z)
  (if (divides? z 3)
      (car (/ z 3))
      (find-base z 2)))

(define (cdr z)
  (if (divides? z 2)
      (cdr (/ z 2))
      (find-base z 3)))
