(define p1 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(2 1) '(0 1)))))

(define p2 (make-polynomial 'x 
                            (attach-tag
                              'sparse 
                              (list '(3 1) '(0 1)))))
(define rf (make-rational p2 p1))
