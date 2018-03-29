(load "logic.scm")

(define mini-db
  (list '(assert! (job (b ben) (computer whiz)))
        '(assert! (job (hacker alyssa) (computer genius)))
        '(assert! (address (b ben) (cambridge (wood st))))
        '(assert! (rule (whiz ?x) (job ?x (computer whiz))))))

(define mini-2
  (list '(job (b ben) (computer whiz))
        '(job (hacker alyssa) (computer genius))
        '(address (b ben) (cambridge (wood st)))
        '(rule (whiz ?x) (job ?x (computer whiz)))))


(populate-db mini-2)
