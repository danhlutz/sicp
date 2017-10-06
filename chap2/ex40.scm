(define nil ())

(define (enumerate-interval start stop)
  (if (> start stop)
      nil
      (cons start (enumerate-interval (+ start 1) stop))))

; prime?

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (define (iter n test)
    (cond ((> (square test) n) n)
          ((divides? n test) test)
          (else (iter n (next test)))))
  (iter n 2))

; flat-map

(define (flat-map proc sequence)
  (fold-right append nil (map proc sequence)))

(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (prime-pair? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-pair?
               (unique-pairs n))))
