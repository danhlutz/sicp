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

(define (or-gate a b output)
  (let ((a-inv (make-wire))
        (b-inv (make-wire))
        (and-inv (make-wire)))
    (inverter a a-inv)
    (inverter b b-inv)
    (and-gate a-inv b-inv and-inv)
    (inverter and-inv output)
    'ok))

; The delay in this or-gate = 
; inverter-delay + and-delay + inverter-delay
