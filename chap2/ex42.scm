(define nil ())

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (safe? k positions)
    (if (= k 1)
        #t
        (and 
          (not-equal-row? (car positions) (cdr positions) k)
          (no-right-diags? (car positions) (cdr positions) k)
          (no-left-diags? (car positions) (cdr positions) k board-size))))
  (define (adjoin-position new-row k rest-of-queens)
    (cons (make-position new-row k) rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
        (list nil)
        (filter 
          (lambda (positions) (safe? k positions))
          (flatmap 
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-position row column)
  (cons row column))

(define (get-row position)
  (car position))

(define (get-column position)
  (cdr position))

(define (not-equal-row? new-position old-positions k)
  (if (= k 1)
      #t
      (and (not (= (get-row new-position)
                   (get-row (car old-positions))))
           (not-equal-row? new-position (cdr old-positions) (- k 1)))))

(define (on-left-diag? pos1 pos2 boardsize)
  (define (flip row boardsize)
    (+ (abs (- row boardsize)) 1))
  (= (- (flip (get-row pos1) boardsize)
        (get-column pos1))
     (- (flip (get-row pos2) boardsize)
        (get-column pos2))))

(define (on-right-diag? pos1 pos2)
  (= (- (get-row pos1)
        (get-column pos1))
     (- (get-row pos2)
        (get-column pos2))))

(define (no-left-diags? new-position old-positions k boardsize)
  (if (= k 1)
      #t
      (and (not (on-left-diag? new-position (car old-positions) boardsize))
           (no-left-diags? new-position 
                           (cdr old-positions) 
                           (- k 1)
                           boardsize))))

(define (no-right-diags? new-position old-positions k)
  (if (= k 1)
      #t
      (and (not (on-right-diag? new-position (car old-positions)))
           (no-right-diags? new-position (cdr old-positions) (- k 1)))))
