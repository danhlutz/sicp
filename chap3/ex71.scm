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

(define (stream-advance start) 
  (cons-stream start (stream-advance (+ 2 start))))

(define evens (stream-advance 2))
(define odds (stream-advance 1))

(define (merge-weighted s t weight)
  (let ((sweight (weight (stream-car s)))
        (tweight (weight (stream-car t))))
    (cond ((< sweight tweight)
           (cons-stream (stream-car s)
                        (merge-weighted (stream-cdr s) t weight)))
          ((> sweight tweight)
           (cons-stream (stream-car t)
                        (merge-weighted s (stream-cdr t) weight)))
          ((and (= sweight tweight)
                (not (eq? (stream-car s) (stream-car t))))
           (cons-stream
             (stream-car s)
             (cons-stream
               (stream-car t)
               (merge-weighted (stream-cdr s) (stream-cdr t) weight))))
          (else
            (cons-stream
              (stream-car s)
              (merge-weighted (stream-cdr s) (stream-cdr t) weight))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs
        (stream-cdr s)
        (stream-cdr t)
        weight)
      weight)))

(define (pair-sum pair)
  (+ (car pair) (cadr pair)))

(define i-plus-j (weighted-pairs integers integers pair-sum))

(define (cube x) (* x x x))

(define (cube-weight pair)
  (let ((i (car pair)) (j (cadr pair)))
    (+ (cube i) (cube j))))

(define (find-same-cubes s weight)
  (let ((current (stream-car s))
        (next (stream-car (stream-cdr s))))
    (if (= (weight current) (weight next))
        (cons-stream 
          (list current next (weight current))
          (find-same-cubes (stream-cdr s) weight))
        (find-same-cubes (stream-cdr s) weight))))

(define ramanujan-numbers
  (find-same-cubes 
    (weighted-pairs integers integers cube-weight)
    cube-weight))

(newline)
(display "First 10 Ramanujan numbers")
(display-stream ramanujan-numbers 10)
