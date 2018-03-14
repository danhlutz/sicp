(load "ch4-3.scm")

(interpret
  '(define phrase
     '(The professor lectures to the student in the class with the cat)))

(start)

;; RESULT 1: The professor is lecturing to the student.
;; The lecturing is happening in class. 
;; and the prof is using the cat to lecture
;; (sentence 
;;   (simple-noun-phrase (article the) (noun professor)) 
;;   (verb-phrase 
;;     (verb-phrase 
;;       (verb-phrase (verb lectures) 
;;       (prep-phrase (prep to) 
;;                    (simple-noun-phrase (article the) (noun student)))) 
;;     (prep-phrase (prep in) 
;;                  (simple-noun-phrase (article the) (noun class)))) 
;;   (prep-phrase (prep with) 
;;                (simple-noun-phrase (article the) (noun cat)))))

;; RESULT 2: The professor is lecturing the student. It's in the class
;; with the cat
;; (sentence (simple-noun-phrase (article the) (noun professor)) 
;;           (verb-phrase 
;;             (verb-phrase (verb lectures) 
;;                          (prep-phrase (prep to) 
;;                          (simple-noun-phrase 
;;                            (article the) (noun student)))) 
;;           (prep-phrase (prep in) 
;;                        (noun-phrase 
;;                          (simple-noun-phrase 
;;                            (article the) (noun class)) 
;;                          (prep-phrase (prep with) 
;;                            (simple-noun-phrase 
;;                              (article the) (noun cat)))))))

;; RESULT 3: The professor is lecturing to the student in the class. 
;; Lecturing with a cat
;; (sentence 
;;   (simple-noun-phrase (article the) (noun professor)) 
;;   (verb-phrase 
;;     (verb-phrase (verb lectures) 
;;                  (prep-phrase 
;;                    (prep to) 
;;                    (noun-phrase 
;;                      (simple-noun-phrase 
;;                        (article the) 
;;                        (noun student)) 
;;                      (prep-phrase 
;;                        (prep in) 
;;                        (simple-noun-phrase (article the) (noun class)))))) 
;;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

;; RESULT 4
;; (sentence 
;;   (simple-noun-phrase (article the) (noun professor)) 
;;   (verb-phrase 
;;     (verb lectures) 
;;     (prep-phrase 
;;       (prep to) 
;;       (noun-phrase 
;;         (noun-phrase 
;;           (simple-noun-phrase 
;;             (article the) (noun student)) 
;;           (prep-phrase 
;;             (prep in) 
;;             (simple-noun-phrase (article the) (noun class)))) 
;;         (prep-phrase 
;;           (prep with) 
;;           (simple-noun-phrase (article the) (noun cat)))))))

;; RESULT 5
;; (sentence 
;;   (simple-noun-phrase (article the) (noun professor)) 
;;   (verb-phrase 
;;     (verb lectures) 
;;     (prep-phrase (prep to) 
;;       (noun-phrase 
;;         (simple-noun-phrase (article the) (noun student)) 
;;         (prep-phrase 
;;           (prep in) 
;;           (noun-phrase 
;;             (simple-noun-phrase (article the) (noun class)) 
;;             (prep-phrase 
;;               (prep with) 
;;               (simple-noun-phrase (article the) (noun cat)))))))))
