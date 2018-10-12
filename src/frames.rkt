#lang racket

(require "vectors.rkt")
(provide frame-coord-map make-frame)

(define (make-frame origin edge1 edge2)
  (make-immutable-hash (list  (cons "origin" origin)
                              (cons "e1" edge1)
                              (cons "e2" edge2))))

(define (origin-frame frame)
  (hash-ref frame "origin"))

(define (edge1-frame frame)
  (hash-ref frame "e1"))

(define (edge2-frame frame)
  (hash-ref frame "e2"))

(define (frame-coord-map frame)
  ;; Origin(frame) + x * Edge1(frame) + y * Edge2(frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcord-vect v)
                           (edge1-frame frame))
               (scale-vect (ycord-vect v)
                           (edge2-frame frame))))))
