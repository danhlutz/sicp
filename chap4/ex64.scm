(load "ms.scm")

(populate-db
  (list '(rule (outranked-by-wrong ?staff-person ?boss)
               (or (supervisor (?staff-person ?boss))
                   (and (outranked-by-wrong ?middle-manager ?boss)
                        (supervisor ?staff-person ?middle-manager))))))

;; querying (outranked-by (Bitdiddle Ben) ?who) causes an infinite loop
;; because the order of operation in AND causes the recursive call
;; to outranked-by to happen before a pattern can be matched 
;; it will only work if a pattern is first suggested in the first half
;; of the AND statement, and the recursive call is second

(populate-db 
  (list '(rule (outranked-by ?staff-person ?boss)
               (or (supervisor (?staff-person ?boss))
                   (and (supervisor ?staff-person ?middle-manager)
                        (outranked-by ?middle-manager? ?boss))))))
