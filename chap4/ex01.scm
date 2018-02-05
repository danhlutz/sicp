(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-done-L-R exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (cons left (list-done-L-R (rest-operands exps) env)))))

(define (list-done-R-L exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-done-R-L (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              right))))


(define (no-operands? exps) (null? exps))

(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))
