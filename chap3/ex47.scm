(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (without-interrupts
    (lambda ()
      (if (car cell)
          true
          (begin (set-car! cell true)
                 false)))))

(define (make-semaphore n)
  (let ((used 0)
        (our-mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (begin (set! used (+ used 1))
                    (if (> used n)
                        (our-mutex 'acquire))))
            ((eq? m 'release)
             (begin (set! used (- used 1))
                    (if (> used n)
                        (our-mutex 'release))))))))
                        
          
    

