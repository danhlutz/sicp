(load "logic.scm")

(define mini-db
  (list '(assert! (job (b ben) (computer whiz)))
        '(assert! (job (hacker alyssa) (computer genius)))
        '(assert! (address (b ben) (cambridge (wood st))))
        '(assert! (rule (whiz ?x) (job ?x (computer whiz))))))

(populate-db mini-db)
