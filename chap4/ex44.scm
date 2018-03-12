(load "amb.scm")

(interpret
  '(define (make-queen column row)
     (cons column row)))

(interpret '(define column car))
(interpret '(define row cdr))
(interpret 
  '(define (reverse-column queen)
     (- 9 (column queen))))

(interpret
  '(define (not-vert-or-horiz? a b)
     (and (not (= (column a) (column b)))
          (not (= (row a) (row b))))))

(interpret 
  '(define (not-right-diagonal? a b)
     (not (= (- (column a) (row a))
             (- (column b) (row b))))))

(interpret
  '(define (not-left-diagonal? a b)
     (not (= (- (reverse-column a) (row a))
             (- (reverse-column b) (row b))))))

(interpret 
  '(define (safe-queens? q1 q2)
     (and (not-vert-or-horiz? q1 q2)
          (not-right-diagonal? q1 q2)
          (not-left-diagonal? q1 q2))))

(interpret
  '(define (safe-cols? new-queen board)
     (if (null? board)
         true
         (and (safe-queens? new-queen (car board))
              (safe-cols? new-queen (cdr board))))))

(interpret '(define a (make-queen 1 1)))
(interpret '(define b (make-queen 8 8)))
(interpret '(define c (make-queen 2 1)))
(interpret '(define d (make-queen 2 4)))
(interpret '(define e (make-queen 4 1)))

(interpret 
  '(define (queens-horrible column-number)
     ;(define (iter column-number)
     (cond ((= column-number 1)
            (cons (make-queen 
                    1 (amb 1 2 3 4 5 6 7 8))
                    '()))
           (else
             (let ((new-queen
                     (make-queen column-number (amb 1 2 3 4 5 6 7 8)))
                   (board (queens-horrible (- column-number 1))))
               (require (safe-cols? new-queen board))
               (cons new-queen board))))))

(interpret
  '(define (queens)
     (let ((board1 (cons (make-queen 1 (amb 1 2 3 4 5 6 7 8)) '()))
           (q2 (make-queen 2 (amb 1 2 3 4 5 6 7 8))))
       (require (safe-cols? q2 board1))
       (let ((board2 (cons q2 board1))
             (q3 (make-queen 3 (amb 1 2 3 4 5 6 7 8))))
         (require (safe-cols? q3 board2))
         (let ((board3 (cons q3 board2))
               (q4 (make-queen 4 (amb 1 2 3 4 5 6 7 8))))
           (require (safe-cols? q4 board3))
           (let ((board4 (cons q4 board3))
                 (q5 (make-queen 5 (amb 1 2 3 4 5 6 7 8))))
             (require (safe-cols? q5 board4))
             (let ((board5 (cons q5 board4))
                   (q6 (make-queen 6 (amb 1 2 3 4 5 6 7 8))))
               (require (safe-cols? q6 board5))
               (let ((board6 (cons q6 board5))
                     (q7 (make-queen 7 (amb 1 2 3 4 5 6 7 8))))
                 (require (safe-cols? q7 board6))
                 (let ((board7 (cons q7 board6))
                       (q8 (make-queen 8 (amb 1 2 3 4 5 6 7 8))))
                   (require (safe-cols? q8 board7))
                   (cons q8 board7))))))))))

; my initial attempt at queens failed miserably, and I'm including it here
; I think the original problem tried to re-solve the problem at every level
; working from the bottom up of the board gave a result in 3.834 secs

(start)
