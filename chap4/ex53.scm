(load "amb1-3.scm")

(interpret
  '(define (prime? x)
     (= x (smallest-divisor x))))

(interpret
  '(define (smallest-divisor x)
     (find-divisor x 2)))

(interpret '(define (square x) (* x x)))

(interpret
  '(define (find-divisor n test-divisor)
     (cond ((> (square test-divisor) n) n)
           ((divides? test-divisor n) test-divisor)
           ((= test-divisor 2)
            (find-divisor n 3))
           (else (find-divisor n (+ test-divisor 2))))))

(interpret
  '(define (divides? a b)
     (= (remainder b a) 0)))

(interpret
  '(define (prime-sum-pair list1 list2)
     (let ((a (an-element-of list1))
           (b (an-element-of list2)))
       (require (prime? (+ a b)))
       (list a b))))

;; The result of evaluating
;; (let ((pairs '()))
;;   (if-fail (let p (prime-sum-pair '(1 3 5 8) '(20 35 110))
;;              (permanent-set! (cons p pairs))
;;              (amb))
;;            pairs)
;; 
;; prodcues '((8 35) (3 110) (3 20))
;; which is a list of all the combinations of the two lists that are prime
;; that's because each time the predicate of the if-fail
;; is evaluated, it fails when (amb) is called at the end of the 
;; expression. However, before the (amb) is called permanent-set! 
;; adds the found prime-sum-pair to the list of pairs
;; once the whole predicate is exhausted and fails
;; the failure mode of if-fail kicks in and returns the final value
;; of pairs
