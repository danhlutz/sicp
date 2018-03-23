;; when running (lives-near ?person-1 ?person-2)
;; each pair of people is returned twice
;; because the pattern is matched twice
;; and only requires that the p1 and p2 in that pattern are distinct

;; it would be possible to make a distinct list
;; by defining a lisp method that compares strings in alphabetical order

(rule (lives-near ?p1 ?p2)
      (and (address ?p1 (?city . ?street1))
           (address ?p2 (?city . ?street2))
           (lisp-action alpha< ?p1 ?p2)))

;; we also don't need to include the (not (same ?p1 ?p2)) check
;; because the alpha< action enforces that the strings are distinct
