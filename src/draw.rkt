#lang racket

(require "gui.rkt")
(require "painters.rkt")
(require "vectors.rkt")

(define (square-and-cross)
  (lambda (frame)
    ((draw-rectangle (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)) frame)
    ((draw-line (make-vect 0.0 0.0)
                (make-vect 1.0 1.0)) frame)
    ((draw-line (make-vect 1.0 0.0)
                (make-vect 0.0 1.0)) frame)
))

(let ((draw (within-frame 500 500)))
  (draw (corner-split (square-and-cross) 5))
  ;; (draw (below
  ;;        (rotate90 (beside (square-and-cross) (square-and-cross)))
  ;;        (rotate90 (beside (square-and-cross) (square-and-cross)))
  ;;        ))

  )
