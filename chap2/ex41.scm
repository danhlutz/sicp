(define nil ())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (k)
             (map (lambda (j) (cons k j))
                  (unique-pairs (- k 1))))
           (enumerate-interval 1 n)))

(define (sum-sequence seq)
  (if (null? seq)
      0
      (+ (car seq) (sum-sequence (cdr seq)))))

(define (triple-sum-finder n s)
  (define (sum-equals? seq)
    (= (sum-sequence seq) s))
  (filter sum-equals?
          (unique-triples n)))
