(define p1 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(1 1) '(0 1)))))

(define p2 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(3 1) '(0 -1)))))

(define p3 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(1 1)))))

(define p4 (make-polynomial 'x
                            (attach-tag
                              'sparse 
                              (list '(2 1) '(0 -1)))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

