#lang racket

(require "vectors.rkt")
(provide frame-coord-map
         make-frame
         make-frame-ext
         get-dc
         origin-values
         origin-frame
         edge1-values
         edge1-frame
         edge2-values
         edge2-frame)

(define (make-frame origin edge1 edge2)
  (make-immutable-hash (list  (cons "origin" origin)
                              (cons "e1" edge1)
                              (cons "e2" edge2))))

(define (make-frame-ext origin edge1 edge2 dc)
  (hash-set (make-frame origin edge1 edge2)
            "draw-context"
            dc))

(define (get-dc frame)
  (hash-ref frame "draw-context"))

(define (origin-frame frame)
  (hash-ref frame "origin"))

(define (origin-values frame)
  (let ((origin (origin-frame frame)))
    (values (xcord-vect origin)
            (ycord-vect origin))))

(define (edge1-frame frame)
  (hash-ref frame "e1"))

(define (edge1-values frame)
  (let ((e1 (edge1-frame frame)))
    (values (xcord-vect e1)
            (ycord-vect e1))))

(define (edge2-frame frame)
  (hash-ref frame "e2"))

(define (edge2-values frame)
  (let ((e2 (edge2-frame frame)))
    (values (xcord-vect e2)
            (ycord-vect e2))))

(define (frame-coord-map frame)
  ;; Origin(frame) + x * Edge1(frame) + y * Edge2(frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcord-vect v)
                           (edge1-frame frame))
               (scale-vect (ycord-vect v)
                           (edge2-frame frame))))))
