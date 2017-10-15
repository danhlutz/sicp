(define (my-equal? a b)
  (if (not (pair? a))
      (eq? a b)
      (and (eq? (car a) (car b))
           (equal? (cdr a) (cdr b)))))
      
      
