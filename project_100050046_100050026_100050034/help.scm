#lang racket

(require racket/draw)
(require racket/gui)

(define help-window (new frame%
                         [label "Help"]
                         [min-height 300]
                         [min-width 645]
                         [stretchable-width #f]
                         [stretchable-height #f]))
(define tab-panel (new tab-panel%
                       [choices (list "Introduction" "How To Use" "How It Works" "About Us")]
                       [parent help-window]
                       [callback (lambda (x y) (tp-callback))]))
(define image-i (read-bitmap "Introduction.png" `png))
(define image-htu (read-bitmap "howtouse.png" `png))
(define image-hiw (read-bitmap "howitworks.png" `png))
(define image-au (read-bitmap "aboutus.png" `png))

(define (tp-callback)
  (let ((n (send tab-panel get-selection)))
    (cond ((= n 0) (begin (send ec refresh) (sleep/yield 0.1) (send ec-dc draw-bitmap image-i 0 0)))
          ((= n 1) (begin (send ec refresh) (sleep/yield 0.1) (send ec-dc draw-bitmap image-htu 0 0)))
          ((= n 2) (begin (send ec refresh) (sleep/yield 0.1) (send ec-dc draw-bitmap image-hiw 0 0)))
          ((= n 3) (begin (send ec refresh) (sleep/yield 0.1) (send ec-dc draw-bitmap image-au 0 0))))))
(define pane (new horizontal-pane% 
                  [parent help-window]
                  [min-height 300]
                  [min-width 600]))

(define ec (new canvas% 
                [parent pane]
                [horiz-margin 5]
                [vert-margin 5]))
(define ec-dc (send ec get-dc))

(define close-button
  (new button%
       [label "Back"]
       [parent help-window]
       [callback (lambda (m x) (begin (send help-window show #f)))]))
(send help-window show #t)
(begin (sleep/yield 0.1) (send ec-dc draw-bitmap image-i 0 0))