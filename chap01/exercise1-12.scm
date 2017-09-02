;; I'm not sure yet how to work with lists in Scheme

;; But here's how I think we want to define this recursively

;; the base case is a one row -- then the answer is the list [1]

;; the recursive case -- the first item of the list should be 1, then
;; each subsequent item of the list equals the item directly above it in the pyramid, plus the preceding list item in the pyramid above. Finally, add a 1 at the end of the list

; example -- level 4

; 1
; 1 1
; 1 2 1
; 1 3 3 1

(define (add-row-pairs list-of-nums)
  (if (= (length list-of-nums) 1)
      list-of-nums
      (append (list (+ (car list-of-nums)
                       (car (cdr list-of-nums))))
              (add-row-pairs (cdr list-of-nums)))))


(define (pascal level)
  (if (= level 1)
      (list 1)
      (append (list 1) (add-row-pairs (pascal (- level 1))))))

