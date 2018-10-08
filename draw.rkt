#lang racket

(require racket/draw)
(require racket/gui/base)

(define frame (new frame%
                   [label "foo"]
                   [width 300]
                   [height 300]))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc draw-rectangle
                             0 10   ; Top-left at (0, 10), 10 pixels down from top-left
                             30 10) ; 30 pixels wide and 10 pixels high
                       (send dc draw-line
                             0 0    ; Start at (0, 0), the top-left corner
                             30 30) ; and draw to (30, 30), the bottom-right corner
                       (send dc draw-line
                             0 30   ; Start at (0, 30), the bottom-left corner
                             30 0)  ; and draw to (30, 0), the top-right corner
                       )]))

;;(define dc (send canvas get-dc))

;; show the canvas first
(send frame show #t)

;; wait for it to be ready
;;(sleep/yield 1)


;; or you could use the paint-callback to draw
;; https://docs.racket-lang.org/gui/windowing-overview.html#%28part._canvas-drawing%29




