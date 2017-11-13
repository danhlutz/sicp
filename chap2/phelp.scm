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
