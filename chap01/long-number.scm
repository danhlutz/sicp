; this program was requested by my 5-year old

(define (long-num len)
  (let ((new-num (random 10)))
    (if (= len 0)
        0
    (+ (* (long-num (- len 1)) 10)
       new-num))))
