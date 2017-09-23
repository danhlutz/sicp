(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (and (< (lower-bound y) 0)
               (> (upper-bound y) 0))
          (or (= (lower-bound y) 0)
              (= (upper-bound y) 0)))
      (error "Cannot divide by interval that spans 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((percent-c (* p c)))
    (make-interval (- c percent-c)
                   (+ c percent-c))))

(define (percent i)
  (/ (width i) (center i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define resistor1 (make-center-percent 6.8 0.01))

(define resistor2 (make-center-percent 4.7 0.02))

(define (print-with-per r)
  (display (center r))
  (display " ohms with ")
  (display (percent r))
  (display "% tolerance")
  (newline))


(newline)
(display "Testing two different methods for computing resistance.")
(newline)
(display "Resistor 1: ")
(print-with-per resistor1)
(display "Resistor 2: ")
(print-with-per resistor2)
(display "Testing R1 * R2 / (R1 + R2): ")
(print-with-per (par1 resistor1 resistor2))
(display "Testing with 1 / ( 1 / R1 + 1 / R2): ")
(print-with-per (par2 resistor1 resistor2))
(display "Second test: R1 combined with R1")
(newline)
(display "Method 1: ")
(print-with-per (par1 resistor1 resistor1))
(display "Method 2: ")
(print-with-per (par2 resistor1 resistor1))
