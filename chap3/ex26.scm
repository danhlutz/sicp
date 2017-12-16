(define (make-entry key value left right)
  (list key value left right))

(define (set-value! entry value)
  (set-car! (cdr entry) value))

(define (set-left! entry new-entry)
  (set-car! (cddr entry) new-entry))

(define (set-right! entry new-entry)
  (set-car! (cdddr entry) new-entry))

(define (get-key entry) (car entry))
(define (get-value entry) (cadr entry))
(define (left entry) (caddr entry))
(define (right entry) (cadddr entry))


(define (insert! key value table)
  (cond ((null? table) (make-entry key value '() '()))
        ((= key (get-key table))
         (set-value! table value))
        ((and (< key (get-key table)) (null? (left table)))
         (set-left! table (make-entry key value '() '())))
        ((and (> key (get-key table)) (null? (right table)))
         (set-right! table (make-entry key value '() '())))
        ((and (< key (get-key table)) (not (null? (left table))))
         (insert! key value (left table)))
        ((and (> key (get-key table)) (not (null? (right table))))
         (insert! key value (right table))))
  'okey-doke)

(define (lookup key table)
  (cond ((null? table) false)
        ((= key (get-key table)) (get-value table))
        ((< key (get-key table))
         (lookup key (left table)))
        ((> key (get-key table))
         (lookup key (right table)))))

