(define nil (list))

(define (square-list-a items)
  (define square (lambda (x) (* x x)))
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-a (cdr items)))))

(define (square-list-b items)
  (map (lambda (x) (* x x)) items))
