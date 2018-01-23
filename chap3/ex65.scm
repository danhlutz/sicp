(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s times)
  (define (iter s times-done)
    (if (> times-done times)
        'done
        (begin (display-line (stream-car s))
               (iter (stream-cdr s) (+ times-done 1)))))
  (iter s 0))

(define (display-line x)
  (newline)
  (display x))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (divide-elements s1 s2)
  (stream-map / s1 s2))

(define (partial-sums s)
  (define (iter s)
    (cons-stream 0 (add-streams s (iter s))))
  (stream-cdr (iter s)))

; SQRT APPROXIMATION AS A STREAM OF GUESSES

(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; pi / 4 = 1 - 1/3 + 1/5 - 1/7

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ; Sn-1
        (s1 (stream-ref s 1))   ; Sn
        (s2 (stream-ref s 2)))  ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; (ln 2) = 1 - 1/2 + 1/3 - 1/4

(define (ln-two-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-two-summands (+ n 1)))))

(define ln-two-stream
  (partial-sums (two-summands 1)))

(newline)
(display "First approx of ln 2 with 500 iterations: ")
(display (stream-ref ln-two-stream 500))

(newline)
(display "Second approx. Euler transform. 10 iters: ")
(display (stream-ref (euler-transform ln-two-stream) 10))

(newline)
(display "Third approx. Tableau transform. 5 iters: ")
(display (stream-ref (accelerated-sequence
                       euler-transform
                       ln-two-stream)
                     5))

(newline)
(display "Scheme evaluation of (log 2): ")
(display (log 2))
