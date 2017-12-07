;; note toward a solution --> 
;; eventually in a cycle, the cdr cdr ... cdr of the cycle equals the cycle
(define (cycle? x)
  (define (cycle-iter? original next)
    (cond ((null? next) #f)
          ((not (pair? next)) #f)
          ((eq? original next) #t)
          (else (cycle-iter? original (cdr next)))))
  (cycle-iter? x (cdr x)))

(define (last-pair? x)
  (if (null? (cdr x))
      x
      (last-pair? (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define cycle (make-cycle (list 'a 'b 'c)))

(define x (list 'a 'b 'c))
(define not-cycle (append x x))


