(define (is-even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-mult-iter a b product)
  (cond ((= b 0) product)
        ((is-even? b) (fast-mult-iter (double a)
                                      (halve b)
                                      product))
        (else (fast-mult-iter a (- b 1) (+ product a)))))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

; simulation
; (fast-mult 20 19)
; (fast-mult-iter 20 19 0)
; (fast-mult-iter 20 18 20)
; (fast-mult-iter 40 9 20)
; (fast-mult-iter 40 8 60)
; (fast-mult-iter 80 4 60)
; (fast-mult-iter 160 2 60)
; (fast-mult-iter 320 1 60)
; (fast-mult-iter 320 0 380)
; 380
