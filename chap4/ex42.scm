(load "amb.scm")

(interpret
  '(define (xor p q)
     (or (and p (not q))
         (and (not p) q))))

(interpret
  '(define (distinct? alist)
     (cond ((null? alist) true)
           ((null? (cdr alist)) true)
           ((member (car alist) (cdr alist)) false)
           (else (distinct? (cdr alist))))))

(interpret
  '(define (liars)
     (let ((betty (amb 1 2 3 4 5))
           (ethel (amb 1 2 3 4 5))
           (joan  (amb 1 2 3 4 5))
           (kitty (amb 1 2 3 4 5))
           (mary  (amb 1 2 3 4 5)))
       (require
         (distinct? (list betty ethel joan kitty mary)))
       (require (xor (= kitty 2) (= betty 3)))
       (require (xor (= ethel 1) (= joan 2)))
       (require (xor (= joan 3)  (= ethel 5)))
       (require (xor (= kitty 2) (= mary 4)))
       (require (xor (= mary 4)  (= betty 1)))
       (list (list 'betty betty)
             (list 'ethel ethel)
             (list 'joan  joan)
             (list 'kitty kitty)
             (list 'mary  mary)))))

;; finds a solution in 1103

(interpret
  '(define (liars-efficient)
     (let ((betty (amb 1 2 3 4 5))
           (kitty (amb 1 2 3 4 5)))
       (require (xor (= kitty 2) (= betty 3)))
       (require (distinct? (list betty kitty)))
       (let ((mary  (amb 1 2 3 4 5)))
         (require (xor (= mary 4)  (= betty 1)))
         (require (xor (= kitty 2) (= mary 4)))
         (require (distinct? (list betty kitty mary)))
         (let ((ethel (amb 1 2 3 4 5))
               (joan  (amb 1 2 3 4 5)))
           (require (xor (= ethel 1) (= joan 2)))
           (require (xor (= joan 3)  (= ethel 5)))
           (require
             (distinct? (list betty ethel joan kitty mary)))
           (list (list 'betty betty)
                 (list 'ethel ethel)
                 (list 'joan  joan)
                 (list 'kitty kitty)
                 (list 'mary  mary)))))))

;; finds the solution in 63 

(start)
