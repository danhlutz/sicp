(load "getput.scm")

(define (apply-generic op . args)
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
  (if (eq? type-tag 'integer)
      contents
      (cons type-tag contents)))

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
          (eq? (type-tag x) 'polynomial))
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

(define (install-polynomial-package)
  ;; internal procedures
  ;; representations of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? v) (symbol? v))

  ;; representations of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
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

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term 
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (=zero-poly? p)
    (null? (term-list p)))

  ;; interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?) 
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
  

; install packages
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-integer-package)
(install-polynomial-package)
