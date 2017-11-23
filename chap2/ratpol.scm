(define old-p1 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(2 1) '(0 1)))))

(define old-p2 (make-polynomial 'x 
                            (attach-tag
                              'sparse 
                              (list '(3 1) '(0 1)))))
(define rf (make-rational old-p2 old-p1))

(define old-p3 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(4 1) '(3 -1) '(2 -2) '(1 2)))))

(define old-p4 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(3 1) '(1 -1)))))

(define p1 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(2 1) '(1 -2) '(0 1)))))

(define p2 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(2 11) '(0 7)))))

(define p3 (make-polynomial 'x
                            (attach-tag 
                              'sparse
                              (list '(1 13) '(0 5)))))

(define Q1 (mul p1 p2))

(define Q2 (mul p1 p3))

(define x1 (make-polynomial 'x
                            (attach-tag 
                              'sparse
                              (list '(4 12) '(3 6) '(2 24)))))

(define x2 (make-polynomial 'x
                            (attach-tag
                              'sparse
                              (list '(0 6)))))
