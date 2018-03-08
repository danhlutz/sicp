(load "amb.scm")

(interpret 
  '(define (time-it proc)
     (let ((start (real-time-clock)))
       (begin (proc)
              (- (real-time-clock) start)))))

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

;; this version makes (expt 5 5) assingments (3125)
;; time in AMB eval: 1060

(interpret
  '(define (optimized-1)
     (let ((baker  (amb 1 2 3 4 5))
           (cooper (amb 1 2 3 4 5)))
       (require (distinct? (list baker cooper)))
       (let ((fletch (amb 1 2 3 4 5)))
         (require (distinct? (list baker cooper fletch)))
         (let  ((miller (amb 1 2 3 4 5)))
           (require (distinct? (list baker cooper fletch miller)))
           (let ((smith  (amb 1 2 3 4 5)))
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
                   (list 'smith smith))))))))

;; by calling distinct at each assingment step, this version only 
;; makes 1,025 assignments
;; time: 534

(interpret
  '(define (optimized-2)
     (let ((smith  (amb 1 2 3 4 5))
           (fletch (amb 1 2 3 4 5)))
       (require (not (= (abs (- smith fletch)) 1)))
       (require (not (= fletch 5)))
       (require (not (= fletch 1)))
       (require (distinct? (list smith fletch)))
       (let ((cooper (amb 1 2 3 4 5)))
         (require (not (= (abs (- fletch cooper)) 1)))
         (require (not (= cooper 1)))
         (require (distinct? (list smith fletch cooper)))
         (let  ((miller (amb 1 2 3 4 5)))
           (require (> miller cooper))
           (require (distinct? (list smith fletch cooper miller)))
           (let ((baker  (amb 1 2 3 4 5)))
             (require (not (= baker 5)))
             (require
               (distinct? (list baker cooper fletch miller smith)))
             (list (list 'baker baker)
                   (list 'cooper cooper)
                   (list 'fletch fletch)
                   (list 'miller miller)
                   (list 'smith smith))))))))

;; this version radically reduces the number of options generated at the
;; first step
;; time: 23

(interpret 
  '(define (test proc description)
     (begin (newline)
            (display "testing in AMB - ")
            (display description)
            (display ": ")
            (time-it proc))))

(start) 
