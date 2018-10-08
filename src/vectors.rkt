#lang racket

(provide add-vect scale-vect xcord-vect ycord-vect)

(define (make-vect x y)
  (cons x y))

(define (xcord-vect v)
  (car v))

(define (ycord-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcord-vect v1) (xcord-vect v2))
   (+ (ycord-vect v1) (ycord-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcord-vect v1) (xcord-vect v2))
   (- (ycord-vect v1) (ycord-vect v2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcord-vect v))
   (* s (ycord-vect v))))
