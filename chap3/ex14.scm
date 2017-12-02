(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; 
; (mystery (list 'a 'b 'c 'd))
; (loop '(a b c d) '())
; (loop '(b c d) '(a))
; (loop '(c d) '(b a))
; (loop '(d) '(c b a))
; (loop '() '(d c b a))
; '(d c b a)

; mystery reverses a list. the answer is built up over time in the y
; parameter of loop like in a tail-recursive process
