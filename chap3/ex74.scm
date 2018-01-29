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
                            test-sig))))))))))))))

;; ALYSSA'S VERSION
; (define (make-zero-crossings input-stream last-value)
;   (cons-stream
;     (sign-change-detector (stream-car input-stream) last-value)
;     (make-zero-crossings (stream-cdr input-stream)
;                          (stream-car input-stream))))
;
; (define zero-crossing (make-zero-crossings sense-data 0))
;


;; EVA'S VERSION

(define zero-crossings 
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
