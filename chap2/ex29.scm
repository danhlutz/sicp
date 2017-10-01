(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

; a. make selectors

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

(define (has-weight? branch) (not (pair? (branch-structure branch))))

; b. total-weight

(define (weigh-branch branch)
  (if (has-weight? branch)
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (weigh-branch (left-branch mobile))
     (weigh-branch (right-branch mobile))))

; c. balanced? 

(define (balanced-branch? branch)
  (if (has-weight? branch)
      #t
      (balanced? (branch-structure branch))))

(define (balanced? mobile)
  (and (= (* (weigh-branch (left-branch mobile))
             (branch-length (left-branch mobile)))
          (* (weigh-branch (right-branch mobile))
             (branch-length (right-branch mobile))))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))

