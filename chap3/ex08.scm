(define f
  (let ((value 13))
    (lambda (x)
      (begin (set! value (+ x value))
             value))))

(load "ex06.scm")

(define (g x) (* (rand 'generate) x))

(newline)
(display "(g 0) ")
(display (g 0))
(newline)
(display "(g 1) ")
(display (g 1))
((rand 'reset) 0)
(newline)
(display "(g 1) ")
(display (g 1))
(newline)
(display "(g 0) ")
(display (g 0))
((rand 'reset) 0)
(newline)
(display "(+ (g 0) (g 1)) ")
(display (+ (g 0) (g 1)))
((rand 'reset) 0)
(newline)
(display "(+ (g 1) (g 0))")
(display (+ (g 1) (g 0)))

; when (g 1) is evaluated first, it equals 1
; (g 0) always equals 0
; since (+ (g 0) (g 1)) equals 1, this shows that the expressions are
; evaluated from back to front
