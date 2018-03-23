;; new rules
(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;; QUERY INPUT
(?x next-to ?y in (1 (2 3) 4))
;; QUERY OUTPUT
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

;; QUERY INPUT
(?x next-to 1 in (2 1 3 1))
;; QUERY OUTPUT
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))
