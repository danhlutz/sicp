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

(define (sign-change-detector current last)
  (cond ((and (< current 0) (>= last 0)) -1)
        ((and (>= current 0) (< last 0)) 1)
        (else 0)))

(define sense-data
  (cons-stream
    1
    (cons-stream
      2
      (cons-stream
        1.5
        (cons-stream
          1
          (cons-stream
            0.5
            (cons-stream
              -0.1
              (cons-stream
                -2
                (cons-stream
                  -3
                  (cons-stream
                    -2
                    (cons-stream
                      -0.5
                      (cons-stream
                        0.2
                        (cons-stream
                          3
                          (cons-stream
                            4
                            sense-data))))))))))))))

(define (smooth s)
  (let ((first (stream-car s))
        (next  (stream-car (stream-cdr s))))
    (let ((avg (/ (+ first next) 2)))
      (cons-stream avg (smooth (stream-cdr s))))))

(define (make-zero-crossings input-stream)
  (define (iter input-stream)
    (let ((smoothed (smooth input-stream)))
      (cons-stream (sign-change-detector
                     (stream-car smoothed)
                     (stream-car (stream-cdr smoothed)))
                   (iter (stream-cdr input-stream)))))
  (iter (cons-stream 0 input-stream)))

(define zero-crossings (make-zero-crossings sense-data))

(define side-by-side
  (stream-map
    (lambda (x y) (list x y))
    sense-data
    zero-crossings))
