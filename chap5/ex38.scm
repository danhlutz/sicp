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

(define (quoted? exp) (tagged-list? exp `quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp `set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp `define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)     ; formal parameters
                   (cddr exp))))   ; body

(define (lambda? exp) (tagged-list? exp `lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons `lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp `if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      `false))

(define (begin? exp) (tagged-list? exp `begin))

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
  (list `procedure parameters body env))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (compound-procedure? exp) (tagged-list? exp `procedure))

(define primitive-procedures
  (list (list `car car)
        (list `cdr cdr)
        (list `cons cons)
        (list `null? null?)
        (list `+ +)
        (list `- -)
        (list `* *)
        (list `/ /)
        (list `= =)
        (list `< <)
        (list `> >)
        (list `list list)
        (list `display display)
        (list `newline newline)
        (list `remainder remainder)
        (list `quotient quotient)
        (list `expt expt)
        (list `eq? eq?)
        (list `equal? equal?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list `primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? proc) (tagged-list? proc `primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;; ENVIRONMENTS

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment `())

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
    (define-variable! `true true initial-env)
    (define-variable! `false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

;; DERIVED EXPRESSIONS

(define (cond? exp) (tagged-list? exp `cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) `else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      `false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn`t last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons `begin seq))

(define (make-if predicate consequent alternative)
  (list `if predicate consequent alternative))

(define (let? exp) (tagged-list? exp `let))

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
      (display (list `compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     `<procedure-environment>))
      (display object)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (exit? exp) (tagged-list? exp `exit))

;; MACHINE HELPERS

(define (empty-arglist) `())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (true? exp) 
  (cond ((eq? exp true) true)
        ((eq? exp `true) true)
        (else false)))

(define (false? exp)
  (cond ((eq? exp false) true)
        ((eq? exp #f) true)
        ((eq? exp `false) true)
        (else false)))

;; MACHINE OPERATIONS

;; THE COMPILER

;; DISPATCH

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((simple-op? exp) (compile-simple-op exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

;; INSTRUCTION-SEQUENCES

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence `() `() `()))

(define (registers-needed s)
  (if (symbol? s) `() (car s)))

(define (registers-modified s)
  (if (symbol? s) `() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;; LINKAGES

(define (compile-linkage linkage)
  (cond ((eq? linkage `return)
         (make-instruction-sequence `(continue) `()
           `((goto (reg continue)))))
        ((eq? linkage `next)
         (empty-instruction-sequence))
        (else
          (make-instruction-sequence `() `()
            `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving `(continue)
    instruction-sequence
    (compile-linkage linkage)))

;; COMPILING SIMPLE EXPRESSION

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence `() (list target)
      `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence `() (list target)
      `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence `(env) (list target)
      `((assign ,target
                (op lookup-variable-value)
                (const ,exp)
                (reg env))))))

;; ASSIGNMENT

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) `val `next)))
    (end-with-linkage linkage
      (preserving `(env)
        get-value-code
        (make-instruction-sequence `(env val) (list target)
          `((perform (op set-variable-value!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

;; DEFINITION

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) `val `next)))
    (end-with-linkage linkage
      (preserving `(env)
        get-value-code
        (make-instruction-sequence `(env val) (list target)
          `((perform (op define-variable!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

;; LABELS

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

;; CONDITIONALS

(define (compile-if exp target linkage)
  (let ((t-branch (make-label `true-branch))
        (f-branch (make-label `false-branch))
        (after-if (make-label `after-if)))
    (let ((consequent-linkage
            (if (eq? linkage `next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) `val `next))
            (c-code
              (compile
                (if-consequent exp) target consequent-linkage))
            (a-code
              (compile (if-alternative exp) target linkage)))
        (preserving `(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence `(val) `()
              `((test (op false?) (reg val))
                (branch (label ,f-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences t-branch c-code)
              (append-instruction-sequences f-branch a-code))
            after-if))))))

;; SEQUENCES

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving `(env continue)
        (compile (first-exp seq) target `next)
        (compile-sequence (rest-exps seq) target linkage))))

;; LAMBDA

(define (make-compiled-procedure entry env)
  (list `compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc `compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label `entry))
        (after-lambda (make-label `after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage `next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
            (make-instruction-sequence `(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence `(env proc argl) `(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op extend-environment)
                   (const ,formals)
                   (reg argl)
                   (reg env))))
      (compile-sequence (lambda-body exp) `val `return))))

;; APPLICATION

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) `proc `next))
        (operand-codes
          (map (lambda (operand) (compile operand `val `next))
               (operands exp))))
    (preserving `(env continue)
      proc-code
      (preserving `(proc continue)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence `() `(argl)
          `((assign argl (const ()))))
        (let ((code-to-get-last-arg
                (append-instruction-sequences
                  (car operand-codes)
                  (make-instruction-sequence `(val) `(argl)
                    `((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving `(env)
                code-to-get-last-arg
                (code-to-get-rest-args
                  (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving `(argl)
            (car operand-codes)
            (make-instruction-sequence `(val argl) `(argl)
              `((assign argl
                 (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving `(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label `primitive-branch))
        (compiled-branch (make-label `compiled-branch))
        (after-call (make-label `after-call)))
    (let ((compiled-linkage
            (if (eq? linkage `next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence `(proc) `()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
              (make-instruction-sequence `(proc argl)
                                         (list target)
                `((assign ,target
                          (op apply-primitive-procedure)
                          (reg proc)
                          (reg argl)))))))
        after-call))))

(define all-regs `(env proc val argl arg1 arg2 continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target `val) (not (eq? linkage `return)))
         (make-instruction-sequence `(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target `val))
              (not (eq? linkage `return)))
         (let ((proc-return (make-label `proc-return)))
           (make-instruction-sequence `(proc) all-regs
             `((assign continue (label ,proc-return))
               (assign val (op compiled-procedure-entry)
                           (reg proc))
               (goto (reg val))
               ,proc-return
               (assign ,target (reg val))
               (goto (label ,linkage))))))
        ((and (eq? target `val) (eq? linkage `return))
         (make-instruction-sequence `(proc continue) all-regs
           `((assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target `val)) (eq? linkage `return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; COMBINING SEQUENCES

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) `())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
              (make-instruction-sequence
                (list-union (list first-reg)
                            (registers-needed seq1))
                (list-difference (registers-modified seq1)
                                 (list first-reg))
                (append `((save ,first-reg))
                        (statements seq1)
                        `((restore ,first-reg))))
              seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-needed seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))

;; OPEN CODING PRIMITIVES

(define simple-ops '(= + - * /))

(define (simple-op? exp)
  (and (list? exp)
       (= (length exp) 3)
       (memq (operator exp) simple-ops)))


(define (spread-arguments exp)
  (let ((operand-list (operands exp)))
    (let ((o1 (car operand-list))
          (o2 (cadr operand-list)))
      (preserving all-regs
         (compile o1 'arg1 'next)
         (compile o2 'arg2 'next)))))

(define (compile-simple-op exp target linkage)
  (let ((compiled-args (spread-arguments exp))
        (op (operator exp)))
    (end-with-linkage 
      linkage
      (append-instruction-sequences
        compiled-args
        (make-instruction-sequence
          '(arg1 arg2)
          '(val)
          `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))

;; MACHINE OPERATIONS

(define compiled-operations 
  (list (list `self-evaluating? self-evaluating?)
        (list `variable? variable?)
        (list `quoted? quoted?)
        (list `text-of-quotation text-of-quotation)
        (list `assignment? assignment?)
        (list `assignment-variable assignment-variable)
        (list `assignment-value assignment-value)
        (list `definition? definition?)
        (list `definition-variable definition-variable)
        (list `definition-value definition-value)
        (list `lambda? lambda?)
        (list `lambda-parameters lambda-parameters)
        (list `lambda-body lambda-body)
        (list `if? if?)
        (list `if-predicate if-predicate)
        (list `if-consequent if-consequent)
        (list `if-alternative if-alternative)
        (list `begin? begin?)
        (list `begin-actions begin-actions)
        (list `last-exp? last-exp?)
        (list `first-exp first-exp)
        (list `rest-exps rest-exps)
        (list `application? application?)
        (list `operator operator)
        (list `operands operands)
        (list `no-operands? no-operands?)
        (list `first-operand first-operand)
        (list `rest-operands rest-operands)
        (list `make-procedure make-procedure)
        (list `compound-procedure? compound-procedure?)
        (list `lookup-variable-value lookup-variable-value)
        (list `empty-arglist empty-arglist)
        (list `adjoin-arg adjoin-arg)
        (list `last-operand? last-operand?)
        (list `primitive-procedure? primitive-procedure?)
        (list `apply-primitive-procedure apply-primitive-procedure)
        (list `procedure-parameters procedure-parameters)
        (list `procedure-environment procedure-environment)
        (list `extend-environment extend-environment)
        (list `procedure-body procedure-body)
        (list `true? true?)
        (list `set-variable-value! set-variable-value!)
        (list `define-variable! define-variable!)
        (list `get-global-environment get-global-environment)
        (list `announce-output announce-output)
        (list `user-print user-print)
        (list `prompt-for-input prompt-for-input)
        (list `exit? exit?)
        (list `cond? cond?)
        (list `cond->if cond->if)
        (list `let? let?)
        (list `let->lambda let->lambda)
        (list `make-compiled-procedure make-compiled-procedure)
        (list `compiled-procedure-env compiled-procedure-env)
        (list `compiled-procedure-entry compiled-procedure-entry)
        (list `list list)
        (list `cons cons)
        (list `false? false?)
        (list `print display)
        (list `= =)
        (list `+ +)
        (list `- -)
        (list `* *)
        (list `/ /)
          ))

;; HELPERS

(define test-code
  `(begin (define (factorial n)
            (if (= n 1)
                1
                (* (factorial (- n 1)) n)))
          (factorial 6)))

(define fib-test
  `(begin (define (fib n)
            (cond ((= n 0) 0)
                  ((= n 1) 1)
                  (else (+ (fib (- n 1))
                           (fib (- n 2))))))
          (fib 10)))

(define fib-iter-test
  `(begin (define (fib-iter n)
            (define (iter a b counter)
              (if (= counter n)
                  b
                  (iter (+ a b) a (+ counter 1))))
            (iter 1 0 0))
          (fib-iter 100)))


(define (compile-with-env exp)
  (append-instruction-sequences
    (make-instruction-sequence
      '()
      '(env)
      '((assign env (op get-global-environment))))
    (compile exp `val `next)
    (make-instruction-sequence '() '()
      '((perform (op print) (reg val))
        (perform (op print-stack-statistics))))))

(define (make-compiled-machine code)
  (make-machine
    all-regs
    compiled-operations
    (statements (compile-with-env code))))
