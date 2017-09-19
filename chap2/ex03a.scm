(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x-point y-point)
  (cons x-point y-point))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (distance start-point end-point)
  (sqrt (+ (square (- (x-point end-point)
                      (x-point start-point)))
           (square (- (y-point end-point)
                      (y-point start-point))))))

(define (len segment)
  (distance (x-point segment) (y-point segment)))

(define (make-rectangle leg1 leg2)
  (cons leg1 leg2))

(define (get-leg1 rectangle)
  (car rectangle))

(define (get-leg2 rectangle)
  (cdr rectangle))

(define (area rectangle)
  (* (len (get-leg1 rectangle))
     (len (get-leg2 rectangle))))

(define (perimeter rectangle)
  (+ (* 2 (len (get-leg1 rectangle)))
     (* 2 (len (get-leg2 rectangle)))))
