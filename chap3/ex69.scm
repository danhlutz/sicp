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

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1) 
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (count-until stream pair)
  (define (iter stream count)
    (if (equal? (stream-car stream) pair)
        count
        (iter (stream-cdr stream) (+ count 1))))
  (iter stream 0))

(define int-pairs (pairs integers integers))

;(define (triples i j k)
;  (cons-stream
;    (list (stream-car i) (stream-car j) (stream-car k))
;    (interleave
;      (stream-map
;        (lambda (x) (list (stream-car i) (stream-car j) x))
;        (stream-cdr k))
;      (interleave
;        (triples i (stream-cdr j) (stream-cdr k))
;        (triples (stream-cdr i) (stream-cdr j) (stream-cdr k))))))

(define (triples i j k)
  (let ((new-pairs (pairs j k)))
    (cons-stream
      (list (stream-car i) (stream-car j) (stream-car k))
      (interleave
        (stream-map (lambda (pair) (cons (stream-car i) pair))
                    new-pairs)
        (triples (stream-cdr i) (stream-cdr j) (stream-cdr k))))))


(define (pythagorean? triple)
  (let ((a (car triple))
        (b (cadr triple))
        (c (caddr triple)))
    (= (+ (square a) 
          (square b))
       (square c))))

(define pyth-triples
  (stream-filter
    pythagorean?
    (triples integers integers integers)))

