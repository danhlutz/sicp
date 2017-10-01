(define nil (list))

(define square (lambda (x) (* x x)))

(define (square-tree-basic tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-basic (car tree))
                    (square-tree-basic (cdr tree))))))

(define a (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(define (square-tree tree)
  (newline)
  (display tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
