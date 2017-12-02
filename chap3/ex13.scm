(define (last-pair? x)
  (if (null? (cdr x))
      x
      (last-pair? (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; if we try to compute (last-pair z), the program enters an infinite loop
; because it never finds a pair with a null cdr
; when it gets to the pair that starts with c, the cdr points back to the
; original pair that starts the list
