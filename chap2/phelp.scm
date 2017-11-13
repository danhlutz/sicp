(define a (make-polynomial 
            'x
            (list (list 3 (make-rational 1 2))
                  (list 2 (make-from-real-imag 2 3))
                  (list 1 3.25)
                  (list 0 17))))

(define b (make-polynomial
            'x
            (list (list 4 (make-rational 2 3))
                  (list 3 (make-rational 3 2))
                  (list 2 (make-from-real-imag 2 0.5))
                  (list 1 18)
                  (list 0 3.27))))

(define c (make-polynomial
            'x
            (list (list 2 3)
                  (list 1 4)
                  (list 0 17))))

(define d (make-polynomial
            'x
            (list (list 3 2)
                  (list 2 10)
                  (list 0 3))))

(define anti-c (make-polynomial
                 'x
                 (list (list 2 -3)
                       (list 1 -4)
                       (list 0 -17))))

(define y1 (make-polynomial
             'y
             (list (list 2 1)
                   (list 1 10)
                   (list 0 3))))

(define y2 (make-polynomial
             'y
             (list (list 2 3)
                   (list 1 23)
                   (list 0 -1))))

(define y3 (make-polynomial
             'y
             (list (list 23 1)
                   (list 13 3)
                   (list 2 14))))

(define z (make-polynomial
            'x
            (list (list 2 y1)
                  (list 1 3)
                  (list 0 13))))

(define z2 (make-polynomial
             'x
             (list (list 2 y2)
                   (list 1 13))))
