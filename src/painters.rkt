#lang racket

(require "frames.rkt")
(require "vectors.rkt")

(require racket/draw)
(require racket/gui/base)
(require pict)

(provide draw-line
         draw-rectangle
         draw-image
         beside
         rotate90
         squash-inwards
         shrink-to-upper-right
         below
         flipped-pairs
         right-split
         up-split
         corner-split
         thetas)

;; Painters
(define (draw-line start end)
  (lambda (frame)
    (let* ([m (frame-coord-map frame)]
           [new-start (m start)]
           [new-end (m end)])
      (send (get-dc frame) draw-line
            (xcord-vect new-start)
            (ycord-vect new-start)
            (xcord-vect new-end)
            (ycord-vect new-end)))))

(define (draw-rectangle start end)
  (lambda (frame)
    (let* ([f (frame-coord-map frame)]
           [coord-start (f start)]
           [coord-end (f end)]
           [dist (sub-vect coord-end coord-start)]
           [min-x (min (xcord-vect coord-start)
                       (xcord-vect coord-end))]
           [min-y (min (ycord-vect coord-start)
                       (ycord-vect coord-end))])
      
      (send (get-dc frame) draw-rectangle
            min-x
            min-y
            (abs (xcord-vect dist))
            (abs (ycord-vect dist))))))

(define (thetas frame)
  (let*-values ([(ox oy)   (origin-values frame)]
                [(e1x e1y) (edge1-values frame)]
                [(e2x e2y) (edge2-values frame)])

    ;; (displayln (origin-frame frame))
    ;; (displayln (edge1-frame frame))
    ;; (displayln (edge2-frame frame))
    
    (cond
      [(< e2x 0) (* pi 0.5)]
      [(< e1x 0) pi]
      [(< e1y 0) (* pi 1.5)]
      [else (* pi 2)])))

(define (draw-image relative-path)
  (lambda (frame)
    (let* ([m (frame-coord-map frame)]
           [coord-start (m (make-vect 0.0 0.0))]
           [coord-end (m (make-vect 1.0 1.0))]
           [dist (sub-vect coord-end coord-start)]
           [min-x (min (xcord-vect coord-start)
                       (xcord-vect coord-end))]
           [min-y (min (ycord-vect coord-start)
                       (ycord-vect coord-end))]           
           [picture (rotate
                     (bitmap relative-path)
                     (thetas frame))]
           [image (pict->bitmap
                   (scale-to-fit
                    picture
                    (abs (xcord-vect dist))
                    (abs (ycord-vect dist))
                    #:mode 'distort))])

      (send (get-dc frame) draw-bitmap image min-x min-y))))

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
       (make-frame-ext new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)
                   (get-dc frame))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (below painter1 painter2)
  (let* ([split-point (make-vect 0.0 0.5)]
         [paint-bottom (transform-painter painter1
                                          (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          split-point)]
         [paint-top (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))])

    (lambda (frame)
      (paint-top frame)
      (paint-bottom frame))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
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

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ([up (up-split painter (- n 1))]
             [right (right-split painter (- n 1))]
             [top-left (beside up up)]
             [bottom-right (below right right)]
             [corner (corner-split painter (- n 1))])
        (beside (below painter top-left)
                (below bottom-right corner)))))
