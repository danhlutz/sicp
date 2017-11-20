(define p1 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(2 1) '(0 1)))))

(define p2 (make-polynomial 'x 
                            (attach-tag
                              'sparse 
                              (list '(3 1) '(0 1)))))
(define rf (make-rational p2 p1))

(define p3 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(4 1) '(3 -1) '(2 -2) '(1 2)))))

(define p4 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(3 1) '(1 -1)))))
