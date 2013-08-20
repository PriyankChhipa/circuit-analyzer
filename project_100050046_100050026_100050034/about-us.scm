#lang racket

(require racket/draw)
(require racket/gui)

(define about-us (new frame%
                         [label "About Us"]
                         [min-height 300]
                         [min-width 645]
                         [stretchable-width #f]
                         [stretchable-height #f]))
(define ec (new canvas% 
                [parent about-us]
                [horiz-margin 5]
                [vert-margin 5]))
(define ec-dc (send ec get-dc))
(define image-au (read-bitmap "aboutus.png" `png))

(define close-button
  (new button%
       [label "Back"]
       [parent about-us]
       [callback (lambda (m x) (begin (send about-us show #f)))]))
(send about-us show #t)
(begin (sleep/yield 0.1) (send ec-dc draw-bitmap image-au 0 0))