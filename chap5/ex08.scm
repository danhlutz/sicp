;; I expect the register a to be 3 when it reaches there

(load "old-mach.scm")

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

;; the content of register a after running the program is in fact 3
;; that shows that in an ambiguous case like this, our simulator
;; will go to the next label
