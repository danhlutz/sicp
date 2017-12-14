(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable 
                          (cons (cons key-2 value)
                                (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define exact-table (make-table equal?))
(define lookup-exact (exact-table 'lookup-proc))
(define insert-exact! (exact-table 'insert-proc!))

(define (close-enough? a b)
  (cond ((or (symbol? a) (symbol? b)) (equal? a b))
        ((and (real? a) (real? b) (< (abs (- a b)) 1)) #t)
        ((and (real? a) (real? b)) #f)
        (else (error "No equality method defined -- CLOSE-ENOUGH"
                     (list a b)))))

(define close-table (make-table close-enough?))
(define lookup-close (close-table 'lookup-proc))
(define insert-close! (close-table 'insert-proc!))

