(load "ex09.scm")

;; this machine should raise an error!

(define exp-machine
  (make-machine
    '(continue b n val)
    (list (list '= =) (list '- -) (list '* *))
    '(start
        (assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (save b)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-loop))
        (goto (label expt-loop))
      after-loop
        (restore b)
        (restore continue)
        (assign val (op *) (reg val) (reg b) (label after-loop))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

