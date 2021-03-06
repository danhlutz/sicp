(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (add-password new-password)
    (begin (set! password-list (cons new-password
                                     password-list))
           "Password added successfully!"))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'add-password) add-password)
          (else "Unknown actin -- MAKE-ACCOUNT"
                m)))
  (define (call-the-cops amount)
    "Max attempts exceeded!!")
  (define password-list (list password))
  (define login
    (let ((login-attempts 7))
      (lambda (pw-attempt m)
        (cond ((in-list? pw-attempt password-list)
               (begin (set! login-attempts 7)
                      (dispatch m)))
              ((and (not (in-list? pw-attempt password-list))
                    (<= login-attempts 1))
               call-the-cops)
              (else
                (begin (set! login-attempts (- login-attempts 1))
                       (lambda (amount) "incorrect password")))))))
  login)

(define (in-list? item list-of-items)
  (cond ((null? list-of-items) #f)
        ((eq? item (car list-of-items)) #t)
        (else (in-list? item (cdr list-of-items)))))

(define (make-joint account old-password new-password)
  (begin ((account old-password 'add-password) new-password)
         account))
