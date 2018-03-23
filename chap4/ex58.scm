;; a person is a big shot if their supervisor does not work in the same 
;; division

(rule (big-shot ?person ?division)
      (and (job ?person (?divsion . ?roles))
           (job ?boss (?boss-division . ?roles2))
           (supervisor ?person ?boss)
           (not (same ?division ?boss-division))))
