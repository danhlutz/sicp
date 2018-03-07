(load "amb.scm")

(interpret 
  '(define (distinct? items)
     (cond ((null? items) true)
           ((null? (cdr items)) true)
           ((member (car items) (cdr items)) false)
           (else (distinct? (cdr items))))))

(interpret
  '(define (multiple-dwelling)
     (let ((baker  (amb 1 2 3 4 5))
           (cooper (amb 1 2 3 4 5))
           (fletch (amb 1 2 3 4 5))
           (miller (amb 1 2 3 4 5))
           (smith  (amb 1 2 3 4 5)))
       (require
         (distinct? (list baker cooper fletch miller smith)))
       (require (not (= baker 5)))
       (require (not (= cooper 1)))
       (require (not (= fletch 5)))
       (require (not (= fletch 1)))
       (require (> miller cooper))
       (require (not (= (abs (- smith fletch)) 1)))
       (require (not (= (abs (- fletch cooper)) 1)))
       (list (list 'baker baker)
             (list 'cooper cooper)
             (list 'fletch fletch)
             (list 'miller miller)
             (list 'smith smith)))))

(interpret
  '(define (more-efficient)
     (let ((baker  (amb 1 2 3 4 5))
           (cooper (amb 1 2 3 4 5))
           (fletch (amb 1 2 3 4 5))
           (miller (amb 1 2 3 4 5))
           (smith  (amb 1 2 3 4 5)))
       (require
         (distinct? (list baker cooper fletch miller smith)))
       (require (not (= (abs (- smith fletch)) 1)))
       (require (not (= (abs (- fletch cooper)) 1)))
       (require (not (= cooper 1)))
       (require (not (= baker 5)))
       (require (not (= fletch 5)))
       (require (not (= fletch 1)))
       (require (> miller cooper))
       (list (list 'baker baker)
             (list 'cooper cooper)
             (list 'fletch fletch)
             (list 'miller miller)
             (list 'smith smith)))))

(start)

;; I think my version of the problem is more efficient
;; the problem starts out with 3,125 different possibilies
;; the distinct requirement reduces that to 120
;; the requirement that a person cannot live on a single floor
;; reduces the possiblities from 120 to 96 (4 * 4 * 3 * 2 *1)
;; but the requirement that a person cannot live on an adjacent floor
;; reduces the possiblities from 120 to 36 
;; (* (+ 3 2 2 2 3) 3 2 1)
