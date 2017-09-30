(define nil (list))

(define (reverse x)
  (if (null? x)
      x
      (append (reverse (cdr x))
              (list (car x)))))

(define (deep-reverse x)
  (cond ((null? x) x)
        ((pair? (car x)) (append (deep-reverse (cdr x))
                                 (list (deep-reverse (car x)))))
        (else (append (deep-reverse (cdr x))
                      (list (car x))))))
