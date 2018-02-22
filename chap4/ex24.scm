(define (time-it proc)
  (let ((start (real-time-clock)))
    (begin (proc)
           (- (real-time-clock) start))))


(define (test proc description)
  (load "meta-0.3.scm")
  (newline)
  (display "testing in analyze environment - ")
  (display description)
  (display (time-it 
             (lambda () (eval proc the-global-environment))))
  (load "meta-0.2.scm")
  (newline)
  (display "testing without analyze - ")
  (display description)
  (display (time-it
             (lambda () (eval proc the-global-environment)))))

(define factorial-proc
  '(begin
     (define (factorial n)
       (if (= n 1)
           1
           (* n (factorial (- n 1)))))
     (factorial 200)))

(define (test-factorial)
  (test factorial-proc "(factorial 200) "))

(define fib-proc
  '(begin
    (define (fib n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1))
                     (fib (- n 2))))))
    (fib 20)))

(define (test-fib)
  (test fib-proc "(fib 20) "))

(define append-proc
  '(begin
     (define (append a b)
       (if (null? a)
           b
           (cons (car a) (append (cdr a) b))))
     (append
       '(1 2 3 4 5 6 7 7 9 9 0 0 0 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)
       '(2 3 7 8 9 7 0 0 1 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9))))

(define (test-append)
  (test append-proc "append 2 lists "))
