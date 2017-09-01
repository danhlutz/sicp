(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
; simulation
; (f 5)
; (f-iter 2 1 0 (- 5 3))
; (f-iter 2 1 0 2# counter)
; (f-iter (+ 2 (* 2 1) (* 3 0)) 2 1 1)
; (f-iter 4 2 1 1)
; (f-iter (+ 4 (* 2 2) (* 3 1)) 4 2 0)
; (f-iter 11 4 2 0)
; (f-iter (+ 11 (* 2 4) (* 3 2)) 11 4 -1)
; (f-iter 25 11 4 -1)

(define (f n)
  (define (f-iter a b c counter)
    (if (< counter 0)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- counter 1))))
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 3))))
