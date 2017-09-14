(define (inc n) (+ n 1))

(define (double f)
  (lambda (x)
    (f (f x))))

; simluation
; (((double (double double)) inc) 5)
; (((double (lambda (x) (double (double x)))) inc) 5)
; (((lambda (x) (lambda (x) (double (double (lambda (x) (double (double x))))))) inc) 5)
; (((double (double (lambda (x) (double (double inc)))))) 5)
; (((double (double (double inc2))) 5)
; (((double (double inc4) ) 5)
; ((double inc8) 5)
; (inc16 5)
; 21

(((double (double double)) inc) 5)
