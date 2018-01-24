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

(define (test-times limit)
  (begin
    (newline)
    (display "How long does it take to get to a given pair?")
    (newline)
    (display "(list 1 ")
    (display limit)
    (display "): ")
    (display (count-until int-pairs (list 1 limit)))
    (newline)
    (display "(list ")
    (display (- limit 1))
    (display " ")
    (display limit)
    (display "): ")
    (display (count-until int-pairs (list (- limit 1) limit)))
    (newline)
    (display "(list ")
    (display limit)
    (display " ")
    (display limit)
    (display "): ")
    (display (count-until int-pairs (list limit limit)))))

; for the first stream, (1 x), after the first pair, every other
; pair is a member
; that means it roughly takes 2x steps to get to (1 x), or about 200 steps t; to get to (1 100)
; every other interleaved pair is a (2 x) pair
; the amount of steps to get to 1 + the car of the pair doubles at each step; that means it would take about 2 ** 99 steps to get to (99 100), 
; and double that or 2 ** 100 steps to get to (100 100)
; it doesn't quite double each time though, because each new interleaved 
; stream starts at the new value of car and doesn't iterate thru the 
; previous values
