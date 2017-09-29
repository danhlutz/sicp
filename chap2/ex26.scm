(define x (list 1 2 3))

(define y (list 4 5 6))

; predictions
; (append x y) -> (1 2 3 4 5 6)
; (cons x y) -> ((1 2 3) 4 5 6)
; (list x y) -> ((1 2 3) (4 5 6)

(newline)
(display (append x y))
(newline )
(display (cons x y))
(newline)
(display (list x y))

