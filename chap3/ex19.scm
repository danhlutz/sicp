(define (cycle? x)
  (define (chase tortoise hare)
    (cond ((null? hare) #f)
          ((not (pair? hare)) #f)
          ((not (pair? tortoise)) #f)
          ((not (pair? (cdr hare))) #f)
          ((eq? tortoise hare) #t)
          (else (chase (cdr tortoise) (cdr (cdr hare) )))))
  (if (not (pair? x))
      #f
      (chase x (cdr x))))

(define (has-cycle? x)
  (if (not (pair? x))
      #f
      (or (cycle? x)
          (has-cycle? (car x))
          (has-cycle? (cdr x)))))

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

(define delayed (cons 'wow cycle))

(define hidden (list 'a 'b cycle 'd))
