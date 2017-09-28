(define square (lambda (x) (* x x)))

(define nil (list))

; first implementation

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; simulation
; (square-list (list 1 2 3))
; (iter (list 1 2 3) nil)
; (iter (list 2 3) (cons 1 nil))
; (iter (list 3) (cons 4 (cons 1 nil)))
; (iter (list) (cons 9 (cons 4 (cons 1 nil))))
; (cons 9 (cons 4 (cons 1 nil)))

; his iterative procedure first applies to (car things). 
; it then squares that and puts in the tail position of a new pair
; so the first will be last and the last first

; second implementation

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; simulation
; (square-list2 (list 1 2 3))
; (iter (list 1 2 3) nil)
; (iter (list 2 3) (cons nil 1))
; (iter (list 3) (cons (cons nil 1) 4))
; (iter (list) (cons (cons (cons nil 1) 4) 9)

; in this case, the answers are in the right order
; but rather than appearing as one list, it is constructed
; as a list of lists
