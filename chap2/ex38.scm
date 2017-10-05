(define nil ())

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; (fold-right / 1 (list 1 2 3))
; (/ 1 (fold-right / 1 (2 3)))
; (/ 1 (/ 2 (fold-right / 1 (3))))
; (/ 1 (/ 2 (/ 3 (fold-right / 1 ()))))
; (/ 1 (/ 2 (/ 3 1)))
; (/ 1 2/3)
; 3/2

; (fold-left / 1 (list 1 2 3))
; (iter 1 (list 1 2 3))
; (iter 1 (list 2 3))
; (iter 1/2 (3))
; 1/6

; (fold-right list nil (list 1 2 3))
; (list 1 (fold-right list nil (list 2 3)))
; (list 1 (list 2 (fold-right list nil (list 3))))
; (list 1 (list 2 (list 3 (fold-right list nil ()))))
; (list 1 (list 2 (list 3 ())))
; (1 (2 (3 ())))

; (fold-left list nil (list 1 2 3))
; (iter nil (list 1 2 3))
; (iter (list () 1) (list 2 3))
; (iter (list (list () 1) 2) (list 3))
; (iter (list (list (list () 1) 2) 3) ())
; (((() 1) 2) 3)

; if an 'op' is commutative then fold-right and fold-left will yield
; equal results. It yields unequal results when the order of application
; matters
