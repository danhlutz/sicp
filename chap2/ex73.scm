(load "getput.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp)  (= exp num)))

; a) We can't assimilate number? and same-variable? into our 
; dispatch system because numbers and symbols are data primitives.
; it's not clear how their type is checked.


(define (install-add-deriv-package)
  ;; internal procedures
  (define (addend x) (car x))
  (define (augend x) (cadr x))
  (define (make-sum a b)
    (cond ((=number? a 0) b)
          ((=number? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b))))

  ;; interface
  (put 'deriv '+ 
       (lambda (exp var) (make-sum (deriv (addend exp) var)
                                   (deriv (augend exp) var))))
  (put 'make-sum 'add-pkg make-sum)
  'done)

(define (install-mul-deriv-package)
  ;; internal procedures
  (define (multiplier x) (car x))
  (define (multiplicand x) (cadr x))
  (define make-sum (get 'make-sum 'add-pkg))
  (define (make-product a b)
    (cond ((or (=number? a 0) (=number? b 0)) 0)
          ((=number? a 1) b)
          ((=number? b 1) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b))))

  ;; interface
  (put 'deriv '*
       (lambda (exp var) 
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var)))))
  
  (put 'make-product 'mul-pkg make-product)
  'done)

(define (install-exponent-deriv-package)
  ;; internal procedures
  (define (base x) (car x))
  (define (power x) (cadr x))
  (define make-product (get 'make-product 'mul-pkg))
  (define (make-exponent b p)
    (cond ((=number? p 0) 1)
          ((=number? p 1) b)
          ((and (number? b) (number? p)) (** b p))
          (else (list '** b p))))

  ;; interface
  (put 'deriv '**
       (lambda (exp var)
         (make-product 
           (make-product (power exp)
                         (make-exponent (base exp)
                                        (- (power exp) 1)))
           (deriv (base exp) var))))
  (put 'make-exponent 'exp-pkg make-exponent)
  'done)

(define (install-trig-deriv-package)
  ;; internal procedures
  (define (trigged x) (car x))
  (define make-product (get 'make-product 'mul-pkg))
  (define (make-sin x)
    (if (number? x)
        (sin x)
        (list 'sin x)))
  (define (make-cos x)
    (if (number? x)
        (cos x)
        (list 'cos x)))

  ;; interface
  (put 'deriv 'sin
       (lambda (exp var)
         (make-product (make-cos (trigged exp))
                       (deriv (trigged exp) var))))
  (put 'deriv 'cos
       (lambda (exp var)
         (make-product -1
                       (make-product (make-sin (trigged exp))
                                     (deriv (trigged exp) var)))))
  'done)

(install-add-deriv-package)
(install-mul-deriv-package)
(install-exponent-deriv-package)
(install-trig-deriv-package)
