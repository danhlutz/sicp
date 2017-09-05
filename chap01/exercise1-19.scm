; To derive p' and q' I'm going to apply the T operator twice to b
; begin with b
; Apply T once
; bp + aq
; Apply T again
; (bp + aq)p + (bq + aq + ap)q
; This reduces to
; b(p**2 + q**2) + a(2pq + q**2)
; p' = p**2 + q**2 and q' = 2pq + q**2

(define (even? n)
  (= (remainder n 2) 0))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
