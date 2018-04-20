(load "machine-0.2.scm")

(define fib-machine
  (make-machine
    '(n continue val)
    (list (list '< <) (list '- -) (list '+ +) (list 'print display))
    '(start
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        ;; set up to computer fib (n-1)
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      afterfib-n-1
        (restore n)
        ;;(restore continue) ;; from older exercise
        ;; set up to computer fib (n-2)
        (assign n (op -) (reg n) (const 2))
        ;;(save continue)    ;; from older exercise
        (assign continue (label afterfib-n-2))
        (save val)
        (goto (label fib-loop))
      afterfib-n-2
        ;(assign n (reg val))
        ;(restore val)
        (restore n) ;; NEW! 
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done
        (perform (op print) (reg val)))))

(define (fib n)
  (define (iter b a counter)
    (if (= counter n)
        a
        (iter (+ b a) b (+ counter 1))))
  (iter 1 0 0))
