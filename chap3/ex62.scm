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

(define (integrate-series s)
  (divide-elements s integers))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 
               (integrate-series 
                 (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                 (scale-stream (stream-cdr s2) (stream-car s1))
                 (mul-series (stream-cdr s1) s2))))

; S * X = 1
; (1 + Sr) * X = 1 
; X + XSr = 1
; X = 1 - X * Sr

(define (invert-unit-series S)
  (cons-stream 
    1
    (scale-stream
      (mul-series (stream-cdr S) (invert-unit-series S)) -1)))

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Can't divide by 0! -- DIV-SERIES" s2)
      (mul-series 
        s1 (invert-unit-series s2))))

(define tan-series 
  (div-series sine-series
              cosine-series))
