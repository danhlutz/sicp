(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; the efficiency of element-of-set? remains O(n)
; and the efficiency of intersection-set remains O(n**2)

; but the efficicency of union-set is now reduced to the efficiency of
; append, which is O(n)
; and the efficiency of adjoin-set is reduced from O(n) to O(1)

; this increase is efficiency could be useful for applications that perform
; a lot of adds or writes to a set, but don't do that many reads of the set
