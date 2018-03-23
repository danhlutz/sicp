;; implement a last-pair rule

;; x is the last-pair of ?items
;; if ?items is not '()
;; and ?rest . ?x is ?items

(rule (same ?x ?x))

(rule (last-pair ?items ?x)
      (and (not (same ?items '()))
           (not (same ?x '()))))

(rule (last-pair (?f . ?y) ?x)
      (last-pair ?y ?x))

;; QUERY-INPUT
(last-pair (3) ?x)
;; OUTPUT
(last-pair (3) (3))

;; INPUT
(last-pair (1 2 3) ?x)
;; OUTPUT
(last-pair (1 2 3) (3))

;; INPUT
(last-pair (2 ?x) (3))
;; OUTPUT
;; I' not sure what the output here would be since
;; it seems like there will be an infinite number of possibilities

;; similarly my rules don't give a good answer to (last-pair ?x (3))
