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

; when two positives intervals are multiplied, 
; the lower bound of the new interval is the product of the two lower bounds; and the upper bound is the product of the two upper bounds

; the lower bound of an interval is equal to (c - c(percent))
; and the upper bound is equal to (c + c(percent))
; the product of the interval with center a and percent with p
; and interval with b and percent q is
; (a - a(p))(b - b(q)), (a + a(p))(b + b(q))
; this reduces to
; ab(1 - p)(1 - q), ab(1 + p)(1 + q)

(define (mul-interval a b)
  (let ((p (percent a))
        (q (percent b))
        (ca (center a))
        (cb (center b)))
    (make-interval (* ca cb (- 1 p) (- 1 q))
                   (* ca cb (+ 1 p) (+ 1 q)))))

