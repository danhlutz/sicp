(define (area x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)))

(define (a2 x1 x2 y1 y2)
  (let ((len (- x2 x1))
        (width (- y2 y1)))
    (* len width)))

(define (a3 x1 x2 y1 y2)
  ((lambda (len width)
     (* len width))
   (- x2 x1) (- y2 y1)))
