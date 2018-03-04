(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

;; we can't find all pythagorean triples by just using 
;; all-integers from n since amb uses depth-first search -- it will never
;; stop searching the values where i is 1

;; i think the approach below could work ???

(define (a-pythagorean-triple low)
  (let ((i (amb low (an-integer-starting-from (+ low 1)))))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
