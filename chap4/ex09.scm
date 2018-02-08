; (for start stop function value)

(define (for->combination exp)
  (define (for-start exp) (cadr exp))
  (define (for-stop exp) (caddr exp))
  (define (for-function) (cadddr exp))
  (define (for-value) (car (cddddr exp)))
  (define (iter start stop function value)
    (if (= start stop)
        (make-application (function value) env)
        (make-application
          (function (iter (+ start 1) stop function value) env))))
  (iter (for-start exp)
        (for-stop exp)
        (for-function exp)
        (for-value exp)))

