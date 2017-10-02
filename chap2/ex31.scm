(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define a (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)
                8
                (list 9 (list 10 11 12) 13)))

(define (square-tree tree) (tree-map square tree))
