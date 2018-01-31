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

(define (lcg seed)
  (let ((a 1103515245)
        (c 1)
        (m (- (expt 2 31) 1)))
    (remainder (+ (* a seed) c) m)))

(define gen (cons-stream 'generate gen))

(define (random-numbers requests)
  (define (iter requests seed)
    (let ((request (stream-car requests)))
    (cond ((eq? request 'generate)
           (cons-stream seed (iter (stream-cdr requests) (lcg seed))))
          ((and (pair? request) (eq? (car request) 'reset))
           (iter (stream-cdr requests) (cadr request))))))
  (iter requests 0))

(define start-stop
  (cons-stream
    'generate
    (cons-stream
      'generate
      (cons-stream
        'generate
        (cons-stream
          'generate
          (cons-stream
            (list 'reset 0)
            (cons-stream
              'generate
              (cons-stream
                'generate
                (cons-stream
                  'generate
                  (cons-stream
                    (list 'reset 0)
                    (cons-stream
                      'generate
                      (cons-stream
                        'generate
                        (cons-stream
                          (list 'reset 0)
                          start-stop)))))))))))))
