#lang racket

(require "frames.rkt")
(require "vectors.rkt")

;; Painters
(define (draw-line start end)
  (lambda (frame)
    (send frame draw-line start end)))

(define (draw-rectangle start end)
  (lambda (frame)
    (send frame draw-rectangle start end)))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (segments->painter segment-list)
  (lambda (frame)
    (let ([coord-mapf (frame-coord-map frame)])
      (for-each
       (lambda (segment)
         (draw-line
          (coord-mapf (start-segment segment))
          (coord-mapf (end-segment segment))))
       segment-list))))

;; Transformers
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ([m (frame-coord-map frame)]
           [new-origin (m origin)])
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let* ([split-point (make-vect 0.5 0.0)]
         [paint-left (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        split-point
                                        (make-vect 0.0 1.0))]
         [paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.5 1.0))])
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))
