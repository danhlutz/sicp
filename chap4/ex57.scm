(load "ms.scm")

(define can-replace
  (list '(rule (can-replace ?person1 ?person2)
           (or (and (job ?person1 ?role)
                    (job ?person2 ?role))
               (and (job ?person1 ?job1)
                    (job ?person2 ?job2)
                    (can-do-job ?job1 ?job2)
                    (not (same ?person1 ?person2)))))))

(populate-db can-replace)

;; All who people who can replace Cy D Fect
;; (can-replace ?person (Fect Cy D))

;; all people who can replace someone being paid more than they are
;; (and (can-replace ?person1 ?person2)
;;      (salary ?person1 ?amt1)
;;      (salary ?person2 ?amt2)
;;      (lisp-value < ?amt1 ?amt2))
