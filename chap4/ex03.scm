(load "getput.scm")

(define (type-tag x)
  (cond ((number? x) 'number)
        ((string? x) 'string)
        ((symbol? x) 'variable)
        ((pair? x) (car x))
        (else (error "No type-tag TAG" x))))

(define (eval exp env)
  (let ((tag (type-tag exp)))
    (let ((proc (get tag)))
      (cond (proc
              (proc exp env))
            ((application? exp)
             (apply (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
            (else
              (error "Unknown expression type -- EVAL" exp))))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))
          


;; TO-DO !!** !! **
        ;((variable? exp) (lookup-variable-value exp env))
        ;((quoted? exp) (text-of-quotation exp))
        ;((assignment? exp) (eval-assignment exp env))
        ;((definition? exp) (eval-definition exp env))
        ;((if? exp) (eval-if exp env))
        ;((lambda? exp)
        ; (make-procedure (lambda-parameters exp)
        ;                 (lambda-body exp)
        ;                 env))
        ;((begin? exp)
        ; (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (install-self-eval-package)
  ;; internal procedures
  (define (identity x env) x)

  ;; interface
  (put 'number identity)
  (put 'string identity)
  'done)

(define (install-variable-package)
  ;; internal
  ;; need to define lookup-variable-value

  ;; interface
  (put 'variable lookup-variable-value)
  'done)
(install-variable-package)

(define (install-quoted-package)
  ;; iternal procedures
  (define (text-of-quotation exp env)
    (cadr exp))

  ;; interface
  (put 'quote text-of-quotation)
  'done)
(install-quoted-package)

(define (install-assignment-package)
  ;; internal procedures
  (define (assignment-variable x)
    (cadr x))
  (define (assignment-value x)
    (caddr x))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assingment-value exp) env)
                         env)
    'ok)

  ;; interface
  (put 'set! eval-assingment)
  'done)
(install-assignment-package)

(define (install-definition-package)
  ;; internal 
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)   ; parameters
                     (cddr exp)))) ; body

  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env))

  ;; interface
  (put 'define eval-definition)
  'ok)
(install-definition-package)

(define (install-lambda-package)
  ;; internals
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (make-procedure parameters body env)
    (list 'procedure parameters body env))
  (define (build-lambda exp env)
    (make-procedure
      (lambda-parameters exp)
      (lambda-body exp)
      env))

  ;; interface
  (put 'lambda build-lambda)
  'done)
(install-lambda-package)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (install-if-package)
  ;; internals 
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? cdddr exp))
        (cadddr exp)
        'false))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
  ;; interface
  (put 'if eval-if)
  'done)
(install-if-package)

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (install-begin-package)
  ;; internals
  (define (begin-actions exp) (cdr exp))
  (define (process-begin exp env)
    (eval-sequence (begin-actions exp) env))

  ;;interface
  (put 'begin process-begin)
  'done)
(install-begin-package)

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (install-cond-package)
  ;; internals
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn'tlast -- COND->IF"
                         clauses))
              (make-if (cond-predicate clause)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))

  ;; interface
  (put 'cond eval-cond)
  'done)
(install-cond-package)
(install-self-eval-package)

