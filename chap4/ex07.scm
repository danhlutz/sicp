(define test1
  (let* ((x 3)
         (y (+ x 2))
         (z (+ x y 5)))
    (* x z)))

(define test2
  (let ((x 3))
    (let ((y (+ x 2)))
      (let ((z (+ x y 5)))
        (* x z)))))

(define (let*-definitions exp) (cadr exp))

(define (let*-body exp) (caddr exp))

(define (let*-def-param def) (car def))
(define (let*-def-value def) (cadr def))

(define (make-let parameter value body)
  (list 'let (list (list parameter value)) body))

(define (let*->nested-lets exp)
  (define (iter definitions body)
    (if (= (length definitions) 1)
        (make-let (let*-def-param (car definitions))
                  (let*-def-value (car definitions))
                  body)
        (make-let (let*-def-param (car definitions))
                  (let*-def-value (car definitions))
                  (iter (cdr definitions) body))))
  (iter (let*-definitions exp) (let*-body exp)))

(let*->nested-lets 
  '(let* ((x 3)
          (y (+ x 2))
          (z (+ x y 5)))
     (* x z)))
