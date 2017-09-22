(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))


(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (define (span? interval)
    (cond ((< (upper-bound interval) 0) -1)
          ((< (lower-bound interval) 0) 0)
          (else 1)))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (= (span? x) 1) (= (span? y) 1))
           (make-interval (* lx ly) (* ux uy)))
          ((and (= (span? x) 1) (= (span? y) 0))
           (make-interval (* ux ly) (* ux uy)))
          ((and (= (span? x) 1) (= (span? y) -1))
           (make-interval (* ux ly) (* lx uy)))
          ((and (= (span? x) -1) (= (span? y) 1))
           (make-interval (* lx uy) (* ux ly)))
          ((and (= (span? x) -1) (= (span? y) 0))
           (make-interval (* lx uy) (* lx ly)))
          ((and (= (span? x) -1) (= (span? y) -1))
           (make-interval (* ux uy) (* lx ly)))
          ((and (= (span? x) 0) (= (span? y) 1))
           (make-interval (* lx uy) (* ux uy)))
          ((and (= (span? x) 0) (= (span? y) -1))
           (make-interval (* ux ly) (* lx ly)))
          (else 
            (make-interval 
              (min (* lx ly) (* lx uy) (* ux ly) (* ux uy))
              (max (* lx ly) (* lx uy) (* ux ly) (* ux uy)))))))

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

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2.0))

; tools for testing with randoms intervals

(define (random-sign)
  (if (< (random 10) 5)
    -
    +))

(define (equal-interval? a b)
  (and (= (lower-bound a)
          (lower-bound b))
       (= (upper-bound a)
          (upper-bound b))))

(define (make-random-intv)
  (let ((a-low ((random-sign) (random 10))))
    (make-interval a-low
                   (+ a-low (random 10)))))

(define (test-harness times)
  (if (= times 0)
      #t
      (let ((a (make-random-intv))
            (b (make-random-intv)))
        (newline)
        (display "Testing with: ")
        (display a)
        (display "*")
        (display b)
        (and (equal-interval? (mul-interval a b)
                              (old-mul-interval a b))
             (test-harness (- times 1))))))

; test the code by running random test 1000 times

(test-harness 1000)
