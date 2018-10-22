#lang racket

(require racket/draw)
(require racket/gui/base)

(require "frames.rkt")
(require "vectors.rkt")

(provide within-frame)

(define (within-frame h w)
  (lambda (f)
    (let* ([frame  (new frame%
                        [label "Picture Language"]
                        [width w]
                        [height h])]
           [canvas (new canvas%
                        [parent frame]
                        [style (list 'transparent)]
                        [paint-callback
                         (lambda (canvas dc)
                           (f (make-frame-ext
                               (make-vect 0.0 0.0)
                               (make-vect w 0.0)
                               (make-vect 0.0 h)
                               dc))
                           )])])
      (send frame show #t))))
