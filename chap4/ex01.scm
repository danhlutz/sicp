(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-L->R exps env)
  (if (no-operands? exps)
      '()
      (begin
        (let ((new-value (eval (first-operand exps) env)))
          (cons new-value (list-L->R (rest-operands exps) env))))))

(define (no-operands? exps) (null? exps))

(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))
