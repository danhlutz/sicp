(load "machine.scm")

(define ambig
  (make-machine
    '(a)
    (list (list 'print display))
    '(start
        (goto (label here))
      here
        (assign a (const 3))
        (goto (label there))
      here
        (assign a (const 4))
        (goto (label there))
      there
        (perform (op print) (reg a)))))

;; defining ambig should raise an error
