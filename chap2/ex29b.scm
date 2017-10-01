; d. new constructors

(define (make-mobile left right)
  (cons left right))

(define (make-branch len structure)
  (cons len structure))

; d. and new selectors

(define (left-branch mobile) (car mobile))

; this changes from (car (cdr mobile)) to just (cdr mobile)
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))

; and this changes from (car (cdr branch)) to just (cdr branch)
(define (branch-structure branch) (cdr branch))

(define (has-weight? branch) (not (pair? (branch-structure branch))))

; b. total-weight --> nothing changed here

(define (weigh-branch branch)
  (if (has-weight? branch)
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (weigh-branch (left-branch mobile))
     (weigh-branch (right-branch mobile))))

; c. balanced? --> nothing changed here

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

