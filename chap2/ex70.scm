(load "ex69.scm")

(define radio-tree
  (generate-huffman-tree '((A 2)
                           (BOOM 1)
                           (GET 2)
                           (JOB 2)
                           (NA 16)
                           (SHA 3)
                           (YIP 9)
                           (WAH 1))))

(define msg
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(newline)
(display "encoding ") 
(display msg)
(newline)
(display (encode msg radio-tree))
(newline)
(display "total bits required " )
(display (length (encode msg radio-tree)))

; this message requires 84 bits to encode
; if a fixed length code was used, each word would require 3 bits
; for 36 words means 108 bits would be required.
; Huffman encoding saves 24 bits
