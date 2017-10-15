#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define outline
  (segment->painter (list (make-segment (make-vect 0 0)
                                        (make-vect 1 0))
                          (make-segment (make-vect 1 0)
                                        (make-vect 1 1))
                          (make-segment (make-vect 1 1)
                                        (make-vect 0 1))
                          (make-segment (make-vect 0 1)
                                        (make-vect 0 0)))))

(define x
  (segment->painter (list (make-segment (make-vect 0 0)
                                        (make-vect 1 1))
                          (make-segment (make-vect 0 1)
                                        (make-vect 1 0)))))

(define diamond
  (segment->painter (list (make-segment (make-vect 0.5 0)
                                        (make-vect 1 0.5))
                          (make-segment (make-vect 1 0.5)
                                        (make-vect 0.5 1))
                          (make-segment (make-vect 0.5 1)
                                        (make-vect 0 0.5))
                          (make-segment (make-vect 0 0.5)
                                        (make-vect 0.5 0)))))
