#lang racket

(require "frames.rkt")

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (draw-line start end) #t)

(define (segments->painter segment-list)
  (lambda (frame)
    (let ([coord-mapf (frame-coord-map frame)])
      (for-each
       (lambda (segment)
         (draw-line
          (coord-mapf (start-segment segment))
          (coord-mapf (end-segment segment))))
       segment-list))))
