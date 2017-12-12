; CONSTRUCTORS
(define (make-deque) (cons '() '()))

(define (make-deq-item value previous next)
  (list value previous next))

; DEQ-ITEM SELECTORS
(define (value deq-item) (car deq-item))
(define (previous deq-item) (cadr deq-item))
(define (next deq-item) (caddr deq-item))

; DEQ-ITEM MUTATORS
(define (set-previous! deq-item new-previous)
  (set-car! (cdr deq-item) new-previous))
(define (set-next! deq-item new-next)
  (set-car! (cddr deq-item) new-next))

; DEQUE SELECTORS
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

; DEQUE PTR MUTATORS
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

; PREDICATE
(define (empty-deque? deque) (null? (front-ptr deque)))

; SELECTORS
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with empty deque" deque)
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with empty deque" deque)
      (value (rear-ptr deque))))

; MUTATORS
(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((new-deq-item (make-deq-item item '() '())))
           (set-front-ptr! deque new-deq-item)
           (set-rear-ptr! deque new-deq-item)))
        (else
          (let ((new-deq-item 
                  (make-deq-item item (rear-ptr deque) '())))
            (set-next! (rear-ptr deque) new-deq-item)
            (set-rear-ptr! deque new-deq-item)))))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((new-deq-item (make-deq-item item '() '())))
           (set-front-ptr! deque new-deq-item)
           (set-rear-ptr! deque new-deq-item)))
        (else
          (let ((new-deq-item
                  (make-deq-item item '() (front-ptr deque))))
            (set-previous! (front-ptr deque) new-deq-item)
            (set-front-ptr! deque new-deq-item)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with empty deque" deque))
        ((null? (next (front-ptr deque))) 
         (set-rear-ptr! deque '()) 
         (set-front-ptr! deque '()))
        (else
          (set-front-ptr! deque (next (front-ptr deque)))
          (set-previous! (front-ptr deque) '()))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with empty deque" deque))
        ((null? (previous (rear-ptr deque)))
         (set-rear-ptr! deque '())
         (set-front-ptr! deque '()))
        (else
          (set-rear-ptr! deque (previous (rear-ptr deque)))
          (set-next! (rear-ptr deque) '()))))

(define (print-deque deque)
  (define (iter deq-item)
    (if (null? deq-item)
        (display " ")
        (begin (display " ")
               (display (value deq-item))
               (iter (next deq-item)))))
  (newline)
  (display "deque:")
  (iter (front-ptr deque)))
