(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc stream)
  (if (stream-null? stream-for-each)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

; => (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; Value: seq
; value of sum is 0

; => (define y (stream-filter even? seq))
; Value: y
; value of sum is 0
;
; => (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; Value: z
; value of sum is 0

; => (stream-ref y 7)
; 136
; value of sum is 136

; => (display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; value of sum is 210


