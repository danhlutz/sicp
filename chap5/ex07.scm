(load "machine.scm")

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
        (assign val (op *) (reg val) (reg b))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

;; using tail recursion and no stack

(define tail-machine
  (make-machine
    '(product b n)
    (list (list '= =) (list '* *) (list '- -) (list 'print display))
    '(start
        (assign product (const 1))
      iter
        (test (op =) (reg n) (const 0))
        (branch (label done))
        (assign product (op *) (reg product) (reg b))
        (assign n (op -) (reg n) (const 1))
        (goto (label iter))
      done
        (perform (op print) (reg product)))))

(define factorial-machine
  (make-machine
    '(continue n val)
    (list (list '= =) (list '- -) (list '* *) (list 'print display))
    '(start
       (assign continue (label fact-done))
      fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
      after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
      base-case
       (assign val (const 1))
       (goto (reg continue))
      fact-done
       (perform (op print) (reg val)))))
