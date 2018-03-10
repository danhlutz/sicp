(define version "amb 0.1")
(define description "AMB evaluator. Based on meta-0.3.scm")

(define apply-in-underlying-scheme apply)

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else 
          (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
                  ;; success condition for this aproc
                  (lambda (arg fail2)
                    (get-args (cdr aprocs)
                              env
                              ;; success continuation for 
                              ;; recursive call to get-args
                              (lambda (args fail3)
                                (succeed (cons arg args)
                                         fail3))
                              fail2))
                  fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else 
          (error "Unknown procedure type -- EXECUTE-APPLICATION"
                 proc))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) 
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) 
      (succeed qval fail))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ; formal parameters
                   (cddr exp))))  ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail) 
      (succeed (make-procedure vars bproc env)
               fail))))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and? exp) (tagged-list? exp 'and))

(define (and-predicates exp) (cdr exp))

(define (and->if exp)
  (define (expand predicates)
    (if (= (length predicates) 1)
        (make-if (car predicates) 'true 'false)
        (make-if (car predicates)
                 (expand (cdr predicates))
                 'false)))
  (expand (and-predicates exp)))

(define (or? exp) (tagged-list? exp 'or))

(define (or-predicates exp) (cdr exp))

(define (or->if exp)
  (define (expand predicates)
    (if (= (length predicates) 1)
        (make-if (car predicates) 'true 'false)
        (make-if (car-predicates)
                 'true
                 (expand (cdr predicates)))))
  (expand (or-predicates exp)))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

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

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                            ; no else clause
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

(define (and? exp) (tagged-list? exp 'and))

(define (and-predicates exp) (cdr exp))

(define (and->if exp)
  (define (expand predicates)
    (if (= (length predicates) 1)
        (make-if (car predicates) 'true 'false)
        (make-if (car predicates) 
                 (expand (cdr predicates))
                 'false)))
  (expand (and-predicates exp)))

(define (or? exp) (tagged-list? exp 'or))

(define (or-predicates exp) (cdr exp))

(define (or->if exp)
  (define (expand predicates)
    (if (= (length predicates) 1) ; last predicate
        (make-if (car predicates) 'true 'false)
        (make-if (car predicates)
                 'true
                 (expand (cdr predicates)))))
  (expand (or-predicates exp)))

(define (let? exp) (tagged-list? exp 'let))

(define (let-definitions exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-defs->parameters definitions)
  (fold-right
    cons '() (map car definitions)))
(define (let-defs->values definitions)
  (fold-right
    cons '() (map cadr definitions)))

(define (let->combination exp)
  (cons (make-lambda 
          (let-defs->parameters (let-definitions exp))  ; parameters
          (let-body exp))                               ; body
        (let-defs->values (let-definitions exp))))      ; values

(define (true? x) (not (eq? x false)))

(define (false? x) 
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (list-let-vars let-vars)
  (map (lambda (x) (list x '(quote *unassigned*))) let-vars))

(define (set-let-vals! let-vars let-vals)
  (map (lambda (x y) (list 'set! x y)) let-vars let-vals))

(define (scan-out-defines body)
  (define (scan let-vars let-vals body)
    (if (definition? (car body))
        (let ((new-def (car body))
              (rest (cdr body)))
          (scan (cons (definition-variable new-def) let-vars)
                (cons (definition-value new-def) let-vals)
                rest))
        (make-let let-vars let-vals body)))
  (define (make-let let-vars let-vals body)
    (if (null? let-vars)
        body
        (list 
          (cons 'let
                (cons (list-let-vars let-vars)
                      (append (set-let-vals! let-vars let-vals)
                              body))))))
  (scan '() '() body))

;; amb expressions

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;; environments

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
             ;; begin ex16 change
             (if (eq? (car vals) '*unassigned*)
                 (error "Variable awaiting assingment -- LOOKUP" var)
                 (car vals)))
             ;; end ex16 change
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (analyze-variable exp)
  (lambda (env succeed fail) 
    (succeed (lookup-variable-value exp env) 
             fail)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()          ;; fail option
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

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

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

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
        (list 'equal? equal?)
        (list 'not not)
        (list 'display display)
        (list 'newline newline)
        (list 'remainder remainder)
        (list 'quotient quotient)
        (list 'list list)
        (list 'abs abs)
        (list 'member member)
        (list 'real-time-clock real-time-clock)
        (list 'expt expt)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; DAN-AMB input:")
(define output-prompt ";;; DAN-AMB value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                         ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(define (interpret input)
  (ambeval input
           the-global-environment
           (lambda (val next-alternative)
             val)
           (lambda () 
             (newline)
             (display "fail")
             (newline))))

(define (install-packages)
  (interpret '(define (require p)
               (if (not p) (amb))))
  (interpret
    '(define (time-it proc)
       (let ((start (real-time-clock)))
         (begin (proc)
                (- (real-time-clock) start)))))
  (interpret
    '(define (map proc items)
       (if (null? items)
           '()
           (cons (proc (car items))
                 (map proc (cdr items))))))
)

(define (start)
  (display "starting DAN-SCHEME v ")
  (display version)
  (newline)
  (display description)
  (install-packages)
  (driver-loop))
