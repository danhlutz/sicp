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
  '(define adjectives '(adjective the a big small green red)))
(interpret
  '(define prepositions '(prep for to in by with)))
(interpret
  '(define conjunctions '(conj and or but)))


(interpret
  '(define (parse-sentence)
     (define (parse-basic-sentence)
       (list 'sentence
             (parse-noun-phrase)
             (parse-verb-phrase)))
     (define (maybe-extend sentence)
       (amb sentence
            (list 'compound
                  sentence
                  (parse-word conjunctions)
                  (parse-basic-sentence))))
     (maybe-extend (parse-basic-sentence))))


(interpret
  '(define (parse-adjective-phrase)
     (amb (parse-word nouns)
          (list 'adj-phrase
                (parse-word adjectives)
                (parse-noun-phrase)))))

(interpret
  '(define (parse-noun-phrase)
     (define (maybe-extend noun-phrase)
       (amb noun-phrase
            (maybe-extend (list 'noun-phrase
                                noun-phrase
                                (parse-prepositional-phrase)))))
     (maybe-extend (parse-adjective-phrase))))

(interpret
  '(define (parse-prepositional-phrase)
     (list 'prep-phrase
           (parse-word prepositions)
           (parse-noun-phrase))))

(interpret
  '(define (parse-verb-phrase)
     (define (maybe-extend verb-phrase)
       (amb verb-phrase
            (maybe-extend (list 'verb-phrase
                               verb-phrase
                               (parse-prepositional-phrase)))))
     (maybe-extend (parse-word verbs))))

;(interpret
; '(define (parse-word word-list)
;    (require (not (null? *unparsed*)))
;    (require (memq (car *unparsed*) (cdr word-list)))
;    (let ((found-word (car *unparsed*)))
;      (set! *unparsed* (cdr *unparsed*))
;      (list (car word-list) found-word))))

(interpret
  '(define (parse-word word-list)
     (let ((words (cdr word-list)))
       (car words))))

(interpret
  '(define *unparsed* '()))

(interpret
  '(define (parse input)
     (set! *unparsed* input)
     (let ((sent (parse-sentence)))
       (require (null? *unparsed*))
       sent)))
