;; more queries

;; name and address of all people supervised by Ben

(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;; everyone who's salary is less than Ben's

(and (salary (Bitdiddle Ben) ?ben-sal)
     (and (salary ?person ?salary)
          (lisp-value < ?ben-sal ?salary)))

;; everyone supervised by someone not in the computer division
(and (supervisor ?person ?manager)
     (not (job ?manager (computer . ?type)))
     (job ?manager ?job))
