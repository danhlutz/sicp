(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (newline)
  (display "elements: ")
  (display elts)
  (display " | n: ")
  (display n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a) partial-tree takes a list of elements to be added to the tree; 
; and the number of elements. This corresponds to the number of leaves
; after the function is called, it first calculates the size of the left 
; tree to be created (left-size), and then recursively calls itself
; until the size is 0. When the size is 0 it returns an empty tree
; and the entire list of elements
; Now the program is ready to make its first leaf -- 
; first it cars the result to get the empty leaf. That will be used to 
; make the left tree. Then it cdrs the result to get the remaining elements.
; it takes the first element of the remaining elements to make the current e
; entry. then it passes the remaining elements and right size to make 
; the right tree. Finally, it takes the resulting tree and adds it 
; up with the remaining elements. The list of remaining elements 
; keeps getting smaller until you end up with a balanced tree

; b) I believe this procedure is O(n * ln(n) ) of efficiency. 
; the procedure must consider each element in the list to place it 
; in the correct position. But at each step it reduces the size of the list 
; of elements to be placed. 

; in addition to the work to place each leaf, we must also factor in the
; the work to get to the base case and add a leaf -- however since n
; is divided by two at each reduction step, this only increases O(ln(n))
