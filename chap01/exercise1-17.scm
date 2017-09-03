(define (multiply a b)
  (if (= b 0)
      0
      (+ a (multiply a (- b 1)))))

(define (double n)
  (+ n n))

(define (even? n) (= (remainder n 2) 0))

(define (halve n)
  (/ n 2))

(define (fast-mul a b)
  (if (= b 0)
      0
      (if (even? b)
          (fast-mul (double a) (halve b))
          (+ a (fast-mul a (- b 1))))))

; simulation 
; (fast-mul 25 13)
; (+ 25 (fast-mul 25 12))
; (+ 25 (fast-mul 50 6))
; (+ 25 (fast-mul 100 3))
; (+ 25 (+ 100 (fast-mul 100 2)))
; (+ 25 (+ 100 (fast-mul 200 1)))
; (+ 25 (+ 100 (+ 200 (fast-mul 200 0))))
; (+ 25 (+ 100 (+ 200 0)))
; (+ 25 (+ 100 200))
; (+ 25 300)
; 325
