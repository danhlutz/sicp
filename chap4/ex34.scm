(define version "0.4.2") 
(define description
  "make CONS, CAR, and CDR lazy")

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->combination exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((application? exp)
         (appl (actual-value (operator exp) env)
               (operands exp)
               env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (appl procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
           procedure 
           (list-of-arg-values arguments env)))     ; changed
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)  ; changed
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

;; LAZY IMPLEMENTATION
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj) 
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
   (cond ((thunk? obj)
          (let ((result (actual-value
                          (thunk-exp obj)
                          (thunk-env obj))))
            (set-car! obj 'evaluated-thunk)
            (set-car! (cdr obj) result)     ; replace exp with value
            (set-cdr! (cdr obj) '())        ; forget unneeded env
            result))
         ((evaluated-thunk? obj)
          (thunk-value obj))
         (else obj)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env)) ;changed
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

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

(define (variable? exp) (symbol? exp))

(define (quoted? exp) 
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

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
            
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

;; (let <defintiions> <body>)
;; (let ((param1 val1) (param2 val2) (param3 val3)) <body>)
;; ((lambda (p1 p2 p3) <body>) val1 val2 val3)

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
 ; (list 'procedure parameters (scan-out-defines body) env))
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

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-definitions exp) (cadr exp))
(define (letrec-body exp) (cddr exp))

(define (letrec->combination exp)
  (let ((let-vars (let-defs->parameters (letrec-definitions exp)))
        (let-vals (let-defs->values (letrec-definitions exp))))
      (cons 'let
            (cons (list-let-vars let-vars)
                  (append (set-let-vals! let-vars let-vals)
                          (letrec-body exp))))))

(define test-letrec
  '(letrec ((even?
             (lambda (n)
               (if (= n 0)
                   true
                   (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (= n 0)
                   false
                   (even? (- n 1))))))
    (if (even? x)
        (* x -13)
        (* x 7))))

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


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list ;(list 'car car)
        ;(list 'cdr cdr)
        ;(list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)
        (list 'display display)
        (list 'newline newline)
        (list 'remainder remainder)
        (list 'quotient quotient)
        (list 'expt expt)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; LAZY-DAN input:")
(define output-prompt ";;; LAZY-DAN value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output 
            (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((and (compound-procedure? object)
              (equal? (procedure-parameters object) '(m))
              (equal? (procedure-body object) '((m x y))))
         (display 
           (force-it (lookup-variable-value 
                       'x
                       (procedure-environment object))))
         (display " . [PROMISE]"))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)
                       '<procedure-env>)))
        (else (display object))))

(define the-global-environment (setup-environment))

(define (install-packages)
  (begin
    (eval '(define (square x) (* x x))
          the-global-environment)
    (eval '(define (cons x y)
             (lambda (m) (m x y)))
          the-global-environment)
    (eval '(define (car p)
             (p (lambda (a b) a)))
          the-global-environment)
    (eval '(define (cdr p)
             (p (lambda (a b) b)))
          the-global-environment)
    (eval '(define (list-ref items n)
             (if (= n 0)
                 (car items)
                 (list-ref (cdr items) (- n 1))))
          the-global-environment)
    (eval '(define (map proc items)
             (if (null? items)
                 '()
                 (cons (proc (car items))
                       (map proc (cdr items)))))
          the-global-environment)
    (eval '(define (scale-list items factor)
             (map (lambda (x) (* x factor))
                  items))
          the-global-environment)
    (eval '(define (add-lists list1 list2)
             (cond ((null? list1) list2)
                   ((null? list2) list1)
                   (else (cons (+ (car list1) (car list2))
                               (add-lists (cdr list1) (cdr list2))))))
          the-global-environment)
    (eval '(define (integral integrand initial-value dt)
             (define int
               (cons initial-value
                     (add-lists (scale-list integrand dt)
                                int)))
             int)
          the-global-environment)
    (eval '(define (solve f y0 dt)
             (define y (integral dy y0 dt))
             (define dy (map f y))
             y)
          the-global-environment)
    (eval '(define x (cons 1 3)) the-global-environment)
    (eval '(display (car x)) the-global-environment)
))

(define (start)
  (begin
    (display "loading DAN-SCHEME version ")
    (display version)
    (newline)
    (display description)
    (newline)
    (display "installing packages ...")
    (install-packages)
    (driver-loop)))
