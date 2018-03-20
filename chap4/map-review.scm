(define (one-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (one-map proc (cdr items)))))

(define (maps proc . items)
  (if (null? (car items))
      '()
      (cons (apply proc (one-map car items))
            (apply maps proc (one-map cdr items)))))
                   
