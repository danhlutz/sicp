(load "ex69.scm")

(define five-tree
  (generate-huffman-tree '((A 1)
                         (B 2)
                         (C 4)
                         (D 8)
                         (E 16))))

(newline)
(display "encoding using a five-tree")
(newline)
(display "A: ")
(display (encode '(A) five-tree))
(newline)
(display "B: ")
(display (encode '(B) five-tree))
(newline)
(display "C: ")
(display (encode '(C) five-tree))
(newline)
(display "D: ")
(display (encode '(D) five-tree))
(newline)
(display "E: ")
(display (encode '(E) five-tree))

(define ten-tree
  (generate-huffman-tree '((A 1)
                           (B 2) 
                           (C 4)
                           (D 8)
                           (E 16)
                           (F 32)
                           (G 64)
                           (H 128)
                           (I 256)
                           (J 512))))

(newline)
(newline)
(display "encoing using a ten-tree")
(newline)
(display "A: ")
(display (encode '(A) ten-tree))
(newline)
(display "E: ")
(display (encode '(E) ten-tree))
(newline)
(display "J: ")
(display (encode '(J) ten-tree))

; when frequencies increase by an order of 2 at each step
; it produces an unbalanced Huffman tree
; the tree requires 1 bit to encode the most frequent symbol
; and n-1 bits to encode the least frequent. 

