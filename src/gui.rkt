#lang racket

(require racket/draw)
(require racket/gui/base)

(require "frames.rkt")
(require "vectors.rkt")

(define (within-frame h w)
  (lambda (f)
    (let* ([frame  (new frame% [label "foo"] [width w] [height h])]
           [canvas (new canvas%
                        [parent frame]
                        [paint-callback
                         (lambda (canvas dc)
                           (f (hash-set
                               (make-frame
                                (make-vect 0.0 0.0)
                                (make-vect 0.0 w)
                                (make-vect 0.0 h))
                               "draw-context"
                               dc))
                           )])])
      (send frame show #t))))
