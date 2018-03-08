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
  '(define (modified-multiple-dwelling)
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
       (require (not (= (abs (- fletch cooper)) 1)))
       (require (> miller cooper))
       (list (list 'baker baker)
             (list 'cooper cooper)
             (list 'fletch fletch)
             (list 'miller miller)
             (list 'smith smith)))))

(start)

;; There are 15 solutions to (modified-multiple-dwelling). try it and see!

;; i made a mistake the first time I ran this. I removed both 
;; adjacent floor rules. By adding back one of them, there are now 
;; 5 solutions
