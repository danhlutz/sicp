(define (make-table) (list '*table*))

(define (lookup keys table)
  (let ((record (find-record keys table)))
    (if record
        (cdr record)
        record)))

(define (find-record keys table)
  (if (or (null? keys) (null? table))
      false
      (let ((subtable (assoc (car keys) (cdr table))))
        (cond ((and subtable (= (length keys) 1))
               subtable)
              ((and subtable (> (length keys) 1))
               (find-record (cdr keys) subtable))
              (else false)))))

(define (insert! keys value table)
  (define (iter keys value)
    (cond ((= 1 (length keys))
           (cons (car keys) value))
          (else
            (list (car keys) (iter (cdr keys) value)))))
  (let ((subtable (find-record (list (car keys)) table)))
    (cond ((and subtable (= (length keys) 1))
           (set-cdr! subtable value))
          ((and (not subtable) (= (length keys) 1))
           (set-cdr! table
                     (cons (cons (car keys) value) (cdr table))))
          ((and subtable (> (length keys) 1))
           (insert! (cdr keys) value subtable))
          ((and (not subtable) (> (length keys) 1))
           (set-cdr! table
                     (cons (iter keys value)
                           (cdr table))))
          (else
            (error "NOT YET DEFINED" (list keys value table)))))
  'ok)
