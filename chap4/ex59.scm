;; Ben adds this to the database

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

(meeting whole-company (Wednesday 4pm))

;; A. Find all meetings on Friday
(meeting ?division (Friday ?time))

;; B. Ask for meetings by specifying a name

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?dt1)
          (and (job ?person (?division . roles))
               (meeting ?division ?dt2))))

(meeting-time (Hacker Alyssa P) ?day-and-time)

;; C. Find Wednesday meetings

(meeting-time (Hacker Alyssa P) (Wednesday . ?time))
