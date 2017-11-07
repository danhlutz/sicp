(define *coercion-table* (make-hash-table))

(define (put-coercion from-type to-type process)
  (hash-table/put! *coercion-table* (list from-type to-type) process))

(define (get-coercion from-type to-type)
  (hash-table/get *coercion-table* (list from-type to-type) #f))
