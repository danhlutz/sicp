(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define b1 (cons 'a 'b))
(define b (cons b1 b1))
; (count-pairs b) returns 3

(define c1 (cons 'a 'b))
(define c2 (cons c1 'e))
(define c (cons c1 c2))
; (count-pairs c) returns 4

(define d1 (cons 'a 'b))
(define d2 (cons d1 d1))
(define d3 (cons d2 d2))
; (count-pairs d3) returns 7


