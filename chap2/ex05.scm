(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (find-base original num-raised)
  (if (= original num-raised)
      1
      (+ 1 (find-base (/ original num-raised)
                      num-raised))))

(define (divides? m n)
  (and (= (remainder m n) 0)
       (> (/ m n) 0)))

(define (car z)
  (if (and (= (remainder z 3) 0)
           (> (/ z 3) 0))
      (car (/ z 3))
      (find-base z 2)))

(define (cdr z)
  (if (divides? z 2)
      (cdr (/ z 2))
      (find-base z 3)))
