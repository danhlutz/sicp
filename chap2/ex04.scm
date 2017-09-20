(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; (car (cons 2 13))
; (car (lambda (m) (m 2 13)))
; ((lambda (m) (m 2 13) ((lambda (p q) p))))
; (lambda (2 13) 2)
; 2 

(define (cdr z)
  (z (lambda (p q) q)))

(define my-pair (cons 13 169))

(newline)
(display "My pair: ")
(display my-pair)
(newline)
(display "My car: ")
(display (car my-pair))
(newline)
(display "My cdr: ")
(display (cdr my-pair))
