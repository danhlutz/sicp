(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged data -- TYPE TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged data -- CONTENTS" datum)))

(define (get-record name file)
  (let ((op (get 'get-record (type-tag file))))
    (op name file)))

(define (get-salary record)
  (let ((op (get 'get-salary (type-tag record))))
    (op record)))

(define (find-employee-record name files)
  (if (null? files)
      false
      (let ((next-file (get-record name (car files))))
        (if (not (eq? next-file #f))
            next-file
            (find-employee-record name (cdr files))))))


(define (install-kronstadt-package)
  ;; internal procedures
  (define (tag record) (attach-tag 'kronstadt-record record))
  (define (get-name-k record) (car record))
  (define (get-salary-k record) (caddr record))
  (define (get-record-k name file)
    (define (iter name records)
      (cond ((null? records) false)
            ((eq? name (get-name-k (car records))) 
             (tag (car records)))
            (else (iter name (cdr records)))))
    (iter name (contents file)))

  ;; interface
  (put 'get-record 'kronstadt get-record-k)
  (put 'get-salary 'kronstadt-record get-salary-k)
  'done)

(define (install-stpete-package)
  ;; internal procedures
  (define (tag record) (attach-tag 'stpete-record record))
  (define (get-name-stpete record) (car record))
  (define (get-record-stpete name file)
    (define (iter name records)
      (cond ((null? records) false)
            ((eq? name (get-name-stpete (car records)))
             (tag (car records)))
            (else (iter name (cdr records)))))
    (iter name (contents file)))
  (define (get-salary-stpete record)
    (cond ((null? record) false)
          ((not (pair? (car record)))
           (get-salary-stpete (cdr record)))
          ((eq? (caar record) 'salary)
           (cdr (car record)))
          (else (get-salary-stpete (cdr record)))))

  ;; interface
  (put 'get-record 'stpete get-record-stpete)
  (put 'get-salary 'stpete-record get-salary-stpete)
  'done)


(load "stpete.scm")
(load "kronstadt.scm")
(load "getput.scm")
(define stpete (attach-tag 'stpete stpete))
(define kronstadt (attach-tag 'kronstadt kronstadt))

(define files (list stpete 
                    kronstadt))

(install-kronstadt-package)
(install-stpete-package)

; d) to add a new unit of this organization, you would need to
; * load the file 
; * attach a new file tag to it
; * add the tagged file to the list of files
; * create and install a package implementing get-record and get-salary
