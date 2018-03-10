(load "amb.scm")

(interpret
  '(define (distinct? alist)
     (cond ((null? alist) true)
           ((null? (cdr alist)) true)
           ((member (car alist) (cdr alist)) false)
           (else (distinct? (cdr alist))))))

(interpret 
  '(define (dads-boat daughter)
     (let ((dadname (dad daughter)))
       (cond ((equal? dadname 'hood) 'gabrielle)
             ((equal? dadname 'hall) 'rosalind)
             ((equal? dadname 'moore) 'lorna)
             ((equal? dadname 'downing) 'melissa)
             (else 'mary-ann)))))

(interpret '(define dad cdr))
(interpret '(define daughter car))

(interpret 
  '(define (dad-of father pairs)
     (if (equal? father (dad (car pairs)))
         (daughter (car pairs))
         (dad-of father (cdr pairs)))))

(interpret
  '(define (yachts)
     (let ((mary-ann (cons 'mary-ann 'moore))
           (gabrielle (cons 'gabrielle
                            (amb 'moore 'dowling 'hall 'hood 'parker)))
           (lorna (cons 'lorna
                        (amb 'moore 'dowling 'hall 'hood 'parker)))
           (rosalind (cons 'rosalind
                           (amb 'moore 'dowling 'hall 'hood 'parker)))
           (melissa (cons 'melissa
                          (amb 'moore 'dowling 'hall 'hood 'parker))))
       (require 
         (distinct? (map 
                      dad
                      (list mary-ann gabrielle lorna rosalind melissa))))
       (require (not (equal? (dad gabrielle) 'hood)))
       (require (not (equal? (dad lorna) 'moore)))
       (require (not (equal? (dad rosalind) 'hall)))
       (require (equal? (dad melissa) 'hood))
       (require (not (equal? (dad gabrielle) 'parker)))
       (require (equal? (dads-boat gabrielle)
                        (dad-of 'parker
                                (list mary-ann
                                      gabrielle
                                      lorna
                                      rosalind
                                      melissa))))
       (list mary-ann gabrielle lorna rosalind melissa))))

(interpret
  '(define (yachts-efficient)
     (let ((mary-ann (cons 'mary-ann 'moore))
           (melissa (cons 'melissa
                          (amb 'moore 'dowling 'hall 'hood 'parker))))
       (require (equal? (dad melissa) 'hood))
       (let ((gabrielle (cons 'gabrielle
                            (amb 'moore 'dowling 'hall 'hood 'parker))))
         (require (not (equal? (dad gabrielle) 'hood)))
         (require (not (equal? (dad gabrielle) 'parker)))
         (require (distinct? 
                    (map dad
                         (list mary-ann melissa gabrielle))))
         (let ((lorna (cons 'lorna
                        (amb 'moore 'dowling 'hall 'hood 'parker))))
           (require (not (equal? (dad lorna) 'moore)))
           (require (distinct? 
                      (map 
                        dad
                        (list mary-ann melissa gabrielle lorna))))
           (let ((rosalind (cons 'rosalind
                           (amb 'moore 'dowling 'hall 'hood 'parker))))
             (require 
               (distinct? (map 
                           dad
                           (list 
                             mary-ann gabrielle lorna rosalind melissa))))
             (require (not (equal? (dad rosalind) 'hall)))
             (require (equal? (dads-boat gabrielle)
                              (dad-of 'parker
                                      (list mary-ann
                                            gabrielle
                                            lorna
                                            rosalind
                                            melissa))))
            (list mary-ann gabrielle lorna rosalind melissa)))))))

;; before testing, I estimate there will be 36 different solutions
;; if we do not require mary-ann to be moore

(interpret
  '(define (mary-ann-wild)
     (let ((mary-ann (cons 'mary-ann
                            (amb 'moore 'dowling 'hall 'hood 'parker)))
           (melissa (cons 'melissa
                          (amb 'moore 'dowling 'hall 'hood 'parker))))
       (require (equal? (dad melissa) 'hood))
       (let ((gabrielle (cons 'gabrielle
                            (amb 'moore 'dowling 'hall 'hood 'parker))))
         (require (not (equal? (dad gabrielle) 'hood)))
         (require (not (equal? (dad gabrielle) 'parker)))
         (require (distinct? 
                    (map dad
                         (list mary-ann melissa gabrielle))))
         (let ((lorna (cons 'lorna
                        (amb 'moore 'dowling 'hall 'hood 'parker))))
           (require (not (equal? (dad lorna) 'moore)))
           (require (distinct? 
                      (map 
                        dad
                        (list mary-ann melissa gabrielle lorna))))
           (let ((rosalind (cons 'rosalind
                           (amb 'moore 'dowling 'hall 'hood 'parker))))
             (require 
               (distinct? (map 
                           dad
                           (list 
                             mary-ann gabrielle lorna rosalind melissa))))
             (require (not (equal? (dad rosalind) 'hall)))
             (require (equal? (dads-boat gabrielle)
                              (dad-of 'parker
                                      (list mary-ann
                                            gabrielle
                                            lorna
                                            rosalind
                                            melissa))))
            (list mary-ann gabrielle lorna rosalind melissa)))))))

;; I was wrong. There are only three solutions when mary ann is not
;; necessarily a moore

(start)
