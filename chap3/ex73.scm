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

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (this-rc current-stream v0)
    (add-streams
      (scale-stream current-stream R)
      (scale-stream
        (integral current-stream v0 dt)
        (/ 1 C))))
  this-rc)

(define zeroes (cons-stream 0 zeroes))
(define unit-signal (cons-stream 1 zeroes))
