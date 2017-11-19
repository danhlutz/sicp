(load "getput.scm")

(define (apply-generic op . args)
; (newline)
; (display "op: ")
; (display op)
; (display " args: ")
; (display args)
  (define (reduce-type x)
;   (newline)
;   (display "in reduce op: ")
;   (display op)
;   (display " x: ")
;   (display x)
    (if (or (eq? op 'add) (eq? op 'sub) (eq? op 'mul) (eq? op 'div)
            (eq? op 'squareroot) (eq? op 'arctan) (eq? op 'sine)
            (eq? op 'cosine))
        (drop x)
        x))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (reduce-type (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a (car args))
                    (b (cadr args)))
                (cond ((higher? a b)
                       (apply-generic op a (raise b)))
                      ((higher? b a)
                       (apply-generic op (raise a) b))
                      (else "Can't raise -- APPLY-GENERIC"
                            (list op a b))))
              (error
                "No method for these types -- APPLY-GENERIC"
                (list op type-tags)))))))

(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'integer) contents)
        ((and (pair? contents) (eq? (car contents) 'scheme-number))
         contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((integer? datum) 'integer)
        ((and (not (integer? datum)) (real? datum)) 'scheme-number)
        ((pair? datum) (car datum))
        (else #f)))

(define (contents datum)
  (cond ((integer? datum) datum) 
        ((and (not (integer? datum)) (real? datum)) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (higher? a b)
  (define (search-tower type1 type2 tower)
    (cond ((null? tower) #f)
          ((eq? type1 (car tower)) #t)
          ((eq? type2 (car tower)) #f)
          (else (search-tower type1 type2 (cdr tower)))))
  (search-tower (type-tag a) (type-tag b) (list 'complex
                                                'scheme-number
                                                'rational
                                                'integer)))

(define type-tower '(complex
                     scheme-number
                     rational
                     integer))

(define (drop x)
  (if (or (eq? (type-tag x) 'integer)
          (eq? (type-tag x) 'polynomial)
          (eq? (type-tag x) 'quot-and-rem))
      x
      (let ((dropped-x (project x)))
        (if (equ? x (raise dropped-x))
            (drop dropped-x)
            x))))

; generic arithmetic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? a b) (apply-generic 'equ? a b))

(define (=zero? a) (apply-generic '=zero? a))

(define (project x) (apply-generic 'project x))

(define square (lambda (x) (mul x x)))

(define (squareroot x) (apply-generic 'squareroot x))

(define (cosine a) (apply-generic 'cosine a))
(define (sine a) (apply-generic 'sine a))
(define (arctan a b) (apply-generic 'arctan a b))

(define (negate a) (apply-generic 'negate a))

(define (install-integer-package)
  ;; internal procedures
  (define (raise-rational n) (make-rational n 1))

  ;; interface
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'make 'integer (lambda (n) (tag n)))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (n) (= n 0)))
  (put 'raise '(integer) raise-rational)
  (put 'squareroot '(integer) sqrt)
  (put 'arctan '(integer integer) atan)
  (put 'cosine '(integer) cos)
  (put 'sine '(integer) sin)
  (put 'negate '(integer) (lambda (n) (- n)))
  'done)

(define (make-integer n) ((get 'make 'integer) n))

(define (install-scheme-number-package)
  ;; internal procedures
  (define (raise-real x)
    (make-from-real-imag x 0))
  (define (project-real x)
    (make-rational (floor->exact x) 1))

  ;; interface
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (n) (= n 0)))
  (put 'raise '(scheme-number) raise-real)
  (put 'project '(scheme-number) project-real)
  (put 'squareroot '(scheme-number) sqrt)
  (put 'arctan '(scheme-number scheme-number) atan)
  (put 'cosine '(scheme-number) cos)
  (put 'sine '(scheme-number) sin)
  (put 'negate '(scheme-number) (lambda (n) (tag (- n))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat? a b)
    (and (= (numer a) (numer b))
         (= (denom a) (denom b))))
  (define (=zero-rat? a)
    (= 0 (numer a)))
  (define (raise-rat m)
    (make-scheme-number (/ (* 1.0 (numer m)) (denom m))))
  (define (project-rat m) (make-integer (numer m)))
  (define (sqrt-rat m) 
    (make-scheme-number (sqrt (/ (numer m) (denom m)))))
  (define (atan-rat a b)
    (arctan (raise-rat a) (raise-rat b)))
  (define (cosine-rat a) (cosine (raise-rat a)))
  (define (sine-rat a) (sine (raise-rat a)))
  (define (negate-rat a)
    (make-rat (negate (numer a)) (denom a)))

  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero-rat?)
  (put 'raise '(rational) raise-rat)
  (put 'project '(rational) project-rat)
  (put 'squareroot '(rational) sqrt-rat)
  (put 'arctan '(rational rational) atan-rat)
  (put 'cosine '(rational) cosine-rat)
  (put 'sine '(rational) sine-rat)
  (put 'negate '(rational) 
       (lambda (x) (tag (negate-rat x))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (squareroot (add (square (real-part z))
                     (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a ) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (squareroot (add (square x)
                           (square y)))
          (arctan y x)))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (equ-complex? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (=zero-complex? z)
    (equ? (magnitude z) 0))
  (define (project-complex z) (make-scheme-number (real-part z)))
  (define (negate-complex z)
    (make-from-real-imag (negate (real-part z))
                         (negate (imag-part z))))
  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) 
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero-complex?)
  (put 'project '(complex) project-complex)
  (put 'negate '(complex) (lambda (z) (tag (negate-complex z))))
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (raise a) (apply-generic 'raise a))

(define (install-dense-poly-package)
  ;; internal procedures
  ;; representations of poly
  (define (make-poly-dense variable term-list)
    (cons variable (contents term-list)))
  (define (variable-dense p) (car p))
  (define (term-list-dense p) (cdr p))

  ;; representations of terms and term lists
  (define (adjoin-term term term-list)
    (define (simplify term-list)
;     (newline)
;     (display "simplify: ")
;     (display term-list)
      (cond ((empty-termlist? term-list) term-list)
            ((equ? 0 (car term-list))
             (simplify (cdr term-list)))
            (else term-list)))
    (define (adjoin-iter term term-list)
;     (newline)
;     (display "adjoin-iter: ")
;     (display term)
;     (display " term-list: ")
;     (display term-list)
;     (newline)
;     (display "order term: ")
;     (display (order-dense term))
      (if (= (length term-list) (order-dense term))
          (cons (coeff-dense term) term-list)
          (cons (coeff-dense term)
                (adjoin-iter 
                  (make-term (- (order-dense term) 1)
                             0)
                  term-list))))
    (simplify (adjoin-iter term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term-dense term-list) 
    (make-term (- (length term-list) 1) 
               (car term-list)))
  (define (rest-terms-dense term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order-dense term) (car term))
  (define (coeff-dense term) (cadr term))

  ;; interface
  (define (tag p) (attach-tag 'dense p))
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly-dense var terms))))
  (put 'variable '(dense) variable-dense)
  (put 'term-list '(dense)
       (lambda (L) (attach-tag 'dense-list (term-list-dense L))))
  (put 'first-term '(dense-list)
       (lambda (L) (attach-tag 'term (first-term-dense L))))
  (put 'rest-terms '(dense-list)
       (lambda (L) (attach-tag 'dense-list (rest-terms-dense L))))
  ;(put 'order '(dense-term) order)
  ;(put 'coeff '(dense-term) coeff)
  (put 'make 'dense 
       (lambda (variable term-list) 
         (tag (make-poly-dense variable term-list))))
  (put 'adjoin-term '(term dense-list)
       (lambda (t L) (attach-tag 'dense-list
                                 (adjoin-term t L))))
  (put 'make-term 'dense 
       (lambda (order coeff) (attach-tag 'term 
                                         (make-term order coeff))))
  (put 'empty-termlist? '(dense-list) empty-termlist?)
  'done)

(define (make-dense-polynomial variable terms)
  (attach-tag 'polynomial 
              ((get 'make 'dense) variable terms)))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representations of poly
  (define (make-poly variable term-list)
    (cons variable (contents term-list)))
  (define (variable-sparse p) (car p))
  (define (term-list-sparse p) (cdr p))
  (define (variable? v) (symbol? v))

  (define (the-empty-termlist-sparse) '())
  (define (empty-termlist-sparse? term-list) 
;   (newline)
;   (display "in empty-termlist-sparse")
;   (display (null? term-list))
    (null? term-list))
  
  ;; representations of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term-sparse term-list) (car term-list))
  (define (rest-terms-sparse term-list) (cdr term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; interface
  (define (tag p) (attach-tag 'sparse p))
  (put 'variable '(sparse) variable-sparse)
  (put 'term-list '(sparse) 
       (lambda (L) 
         (attach-tag 'sparse-list (term-list-sparse L))))
  (put 'first-term '(sparse-list) 
       (lambda (t) 
         (attach-tag 'term (first-term-sparse t))))
  (put 'rest-terms '(sparse-list) 
       (lambda (T) 
         (attach-tag 'sparse-list (rest-terms-sparse T))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'adjoin-term '(term sparse-list) 
       (lambda (term term-list) 
         (attach-tag 'sparse-list (adjoin-term term term-list))))
  (put 'make 'sparse 
       (lambda (variable term-list) (tag (make-poly variable term-list))))
  (put 'make-term 'sparse
       (lambda (order coeff) 
         (attach-tag 'term (make-term order coeff))))
  (put 'empty-termlist? '(sparse-list) empty-termlist-sparse?)
  (put 'the-empty-termlist 'sparse
     (attach-tag 'sparse-list (the-empty-termlist-sparse)))
  'done)

(define (install-poly-math-package)
  ;; import from sparse package
  (define (make-poly variable term-list)
;   (newline)
;   (display "in make-poly term-list type = ")
;   (display (type-tag term-list))
    (if (eq? (type-tag term-list) 'dense-list)
        ((get 'make 'dense) variable term-list)
        ((get 'make 'sparse) variable term-list)))
  (define (make-term order coeff)
    ((get 'make-term 'sparse) order coeff))
  (define (the-empty-termlist)
    (get 'the-empty-termlist 'sparse))

  ;; arithmetic operations
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
;   (newline)
;   (display "add-terms: ")
;   (display L1)
;   (display " + ")
;   (display L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else 
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in the same var -- SUB-POLY"
               (list p1 p2))))
  (define (sub-terms L1 L2)
    (define (negate-term t)
      (make-term (order t) (negate (coeff t))))
    (cond ((empty-termlist? L2) L1)
          ((empty-termlist? L1)
           (adjoin-term (negate-term (first-term L2))
                        (sub-terms L1 (rest-terms L2))))
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (sub-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       (negate-term t2)
                       (sub-terms L1 (rest-terms L2))))
                    (else 
                      (adjoin-term
                        (make-term (order t1)
                                   (sub (coeff t1) (coeff t2)))
                        (sub-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
;   (newline)
;   (display "mul-poly: ")
;   (display p1)
;   (display " * ")
;   (display p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
;   (newline)
;   (display "mul-terms: ")
;   (display L1)
;   (display " * ")
;   (display L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
;   (newline)
;   (display "mul-term-by-all-terms: ")
;   (display t1)
;   (display " * ")
;   (display L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term 
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (=zero-poly? p)
    (null? (term-list p)))
  (define (negate-poly p)
    (sub-poly (make-poly (variable p) '())
              p))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (let ((quot (car result))
                (rem (cadr result)))
            (list (tag (make-poly (variable p1) quot))
                  (tag (make-poly (variable p1) rem)))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (sub (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms
                          (sub-terms 
                            L1
                            (mul-terms 
                              L2
                              (adjoin-term
                                (make-term new-o new-c)
                                (the-empty-termlist))))
                          L2)))
                  (let ((quot (car rest-of-result))
                        (rem (cadr rest-of-result)))
                    (list (adjoin-term 
                            (make-term new-o new-c)
                            quot)
                          rem))))))))

  ;; interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (attach-tag 'quot-and-rem (div-poly p1 p2))))
  (put 'negate '(polynomial) 
       (lambda (p) (tag (negate-poly p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))
(define (first-term L) (apply-generic 'first-term L))
(define (rest-terms L) (apply-generic 'rest-terms L))
(define (order t) (apply-generic 'order t))
(define (coeff t) (apply-generic 'coeff t))
(define (empty-termlist? T) (apply-generic 'empty-termlist? T))
(define (adjoin-term t term-list) (apply-generic 'adjoin-term t term-list))

(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

; install packages
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-integer-package)
(install-polynomial-package)
(install-poly-math-package)
(install-dense-poly-package)
