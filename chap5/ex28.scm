(load "machine-0.4.scm")

;; EXPRESSION TYPES AND SELECTORS

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)     ; formal parameters
                   (cddr exp))))   ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; PROCEDURES

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (compound-procedure? exp) (tagged-list? exp 'procedure))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)
        (list 'list list)
        (list 'display display)
        (list 'newline newline)
        (list 'remainder remainder)
        (list 'quotient quotient)
        (list 'expt expt)
        (list 'eq? eq?)
        (list 'equal? equal?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;; ENVIRONMENTS

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound varaible -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

;; DERIVED EXPRESSIONS

(define (cond? exp) (tagged-list? exp 'cond))

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
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (let? exp) (tagged-list? exp 'let))

(define (let-body exp) (cddr exp))

(define (let-definitions exp) (cadr exp))

(define (let-parameters let-defs)
  (map car let-defs))

(define (let-values let-defs)
  (map cadr let-defs))

(define (let->lambda exp)
  (let ((definitions (let-definitions exp)))
    (let ((params (let-parameters definitions))
          (values (let-values definitions)))
      (cons (make-lambda params (let-body exp))
            values))))

;; ERROR MESSAGES
(define unknown-expression-type-error
  "Unknown expression type -- ECEVAL")

;; REPL OPERATIONS

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-environment>))
      (display object)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (exit? exp) (tagged-list? exp 'exit))

;; MACHINE HELPERS

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (true? exp) 
  (cond ((eq? exp true) true)
        ((eq? exp 'true) true)
        (else false)))

(define (no-more-exps? seq) (null? seq))

;; THE MACHINE

(define eceval-operations 
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'text-of-quotation text-of-quotation)
        (list 'assignment? assignment?)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'definition? definition?)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'lambda? lambda?)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'if? if?)
        (list 'if-predicate if-predicate)
        (list 'if-consequent if-consequent)
        (list 'if-alternative if-alternative)
        (list 'begin? begin?)
        (list 'begin-actions begin-actions)
        (list 'last-exp? last-exp?)
        (list 'first-exp first-exp)
        (list 'rest-exps rest-exps)
        (list 'application? application?)
        (list 'operator operator)
        (list 'operands operands)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'rest-operands rest-operands)
        (list 'make-procedure make-procedure)
        (list 'compound-procedure? compound-procedure?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)
        (list 'last-operand? last-operand?)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'true? true?)
        (list 'set-variable-value! set-variable-value!)
        (list 'define-variable! define-variable!)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'prompt-for-input prompt-for-input)
        (list 'exit? exit?)
        (list 'cond? cond?)
        (list 'cond->if cond->if)
        (list 'let? let?)
        (list 'let->lambda let->lambda)
        (list 'no-more-exps? no-more-exps?)
          ))

(define eceval
  (make-machine
    '(exp env val proc argl continue unev)
    eceval-operations
    '(
      read-eval-print-loop
        (perform (op initialize-stack))
        (perform
          (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
      print-result
        (perform (op print-stack-statistics))
        (perform
          (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))
      unknown-procedure-type
        (restore continue)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))
      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op exit?) (reg exp))
        (branch (label ev-done))
        (test (op cond?) (reg exp))
        (branch (label ev-cond))
        (test (op let?) (reg exp))
        (branch (label ev-let))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
      ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
      ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        (goto (reg continue))
      ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))
      ev-lambda
        (assign unev (op lambda-parameters) (reg exp))
        (assign exp (op lambda-body) (reg exp))
        (assign val (op make-procedure)
                    (reg unev) (reg exp) (reg env))
        (goto (reg continue))
      ev-application
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
      ev-appl-did-operator
        (restore unev)  ;; the operands
        (restore env)
        (assign argl (op empty-arglist))
        (assign proc (reg val))
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
      ev-appl-operand-loop
        (save argl)
        (assign exp (op first-operand) (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
      ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
      ev-appl-last-arg
        (assign continue (label ev-appl-accumulate-last-arg))
        (goto (label eval-dispatch))
      ev-appl-accumulate-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))
      apply-dispatch
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))
      primitive-apply
        (assign val (op apply-primitive-procedure)
                    (reg proc)
                    (reg argl))
        (restore continue)
        (goto (reg continue))
      compound-apply
        (assign unev (op procedure-parameters) (reg proc))
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment)
                    (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))
      ev-begin
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))
      ev-sequence
        (test (op no-more-exps?) (reg unev))
        (branch (label ev-sequence-end))
        (assign exp (op first-exp) (reg unev))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
      ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
      ev-sequence-end
        (restore continue)
        (goto (reg continue))
      ev-if
        (save exp)
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch))
      ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
      ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
      ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
      ev-cond
        (assign exp (op cond->if) (reg exp))
        (goto (label eval-dispatch))
      ev-let
        (assign exp (op let->lambda) (reg exp))
        (goto (label eval-dispatch))
      ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev)
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch))
      ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform
          (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
      ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev)
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch))
      ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform 
          (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
      ev-done
        (perform (op announce-output) (const "Quitting evaluator"))
      )))

