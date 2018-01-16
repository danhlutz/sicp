(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-advance start)
  (cons-stream start (stream-advance (+ 2 start))))

(define evens (stream-advance 2))
(define odds (stream-advance 1))
(define ones (cons-stream 1 ones))
