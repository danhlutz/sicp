(define (inverter input output)
  (define (invert-output)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-output)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" (list s1 s2)))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" (list s1 s2)))))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-date d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry a-list b-list s-list c)
  (define (iter a-list b-list s-list c-out)
    (if (null? a-list)
        'ok
        (let ((c-internal (make-wire)))
          (begin (full-adder (car a-list)
                             (car b-list)
                             c-internal
                             (car s-list)
                             c-out)
                 (iter (cdr a-list)
                       (cdr b-list)
                       (cdr s-list)
                       c-internal)))))
  (iter a-list b-list s-list c))

; total delays in propagation
; half-adder delay = 2 * AND + INV
; full-adder = HALF + OR = 2 * AND + INV + OR
; ripple-carry-adder = N * (2 * AND + INV + OR)

          

    
