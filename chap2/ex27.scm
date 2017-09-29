(define nil (list))

(define (reverse x)
  (define (iter items result)
    (if (= (length items) 0)
        result
        (iter (cdr items) (cons (car items)
                                result))))
  (iter x nil))



(define (deep-reverse x)
  (define (iter items result)
    (if (= (length items) 0)
        result
        (iter (cdr items) (cons (deep-reverse (car items))
                                result))))
  (if (not (pair? x))
      x
      (iter x nil)))
