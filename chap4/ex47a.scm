(load "amb.scm")

(interpret
  '(define (memq to_find items)
     (cond ((null? items) false)
           ((equal? to_find (car items)) items)
           (else (memq to_find (cdr items))))))

(interpret
  '(define nouns '(noun student professor cat class)))
(interpret 
  '(define verbs '(verb studies lectures eats sleeps)))
(interpret 
  '(define articles '(article the a)))
(interpret
  '(define prepositions '(prep for to in by with)))

(interpret
  '(define (parse-sentence)
     (list 'sentence
           (parse-noun-phrase)
           (parse-verb-phrase))))

(interpret
  '(define (parse-simple-noun-phrase)
     (list 'simple-noun-phrase
           (parse-word articles)
           (parse-word nouns))))

(interpret
  '(define (parse-noun-phrase)
     (define (maybe-extend noun-phrase)
       (amb noun-phrase
            (maybe-extend (list 'noun-phrase
                                noun-phrase
                                (parse-prepositional-phrase)))))
     (maybe-extend (parse-simple-noun-phrase))))

(interpret
  '(define (parse-prepositional-phrase)
     (list 'prep-phrase
           (parse-word prepositions)
           (parse-noun-phrase))))

;(interpret
;  '(define (parse-verb-phrase)
;     (define (maybe-extend verb-phrase)
;       (amb verb-phrase
;            (maybe-extend (list 'verb-phrase
;                               verb-phrase
;                               (parse-prepositional-phrase)))))
;     (maybe-extend (parse-word verbs))))

(interpret
  '(define (parse-verb-phrase)
     (amb (parse-word verbs)
          (list 'verb-phrase
                (parse-verb-phrase)
                (parse-prepositional-phrase)))))

(interpret
  '(define (parse-word word-list)
     (require (not (null? *unparsed*)))
     (require (memq (car *unparsed*) (cdr word-list)))
     (let ((found-word (car *unparsed*)))
       (set! *unparsed* (cdr *unparsed*))
       (list (car word-list) found-word))))

(interpret
  '(define *unparsed* '()))

(interpret
  '(define (parse input)
     (set! *unparsed* input)
     (let ((sent (parse-sentence)))
       (require (null? *unparsed*))
       sent)))

;; Louis's idea doesn't work.
;; because the evaluator checks the order from left to right
;; with a verb phrase that contains a preposition, it gets stuck in an
;; infinite loop by calling parse-verb-phrase successively
