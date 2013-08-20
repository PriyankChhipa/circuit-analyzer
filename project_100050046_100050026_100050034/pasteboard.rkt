#lang racket

(require racket/gui)
(require racket/mpair)


(require "project.rkt")
(require "gui-source.rkt")
(require "convert-gui-code.rkt")

(define orientation 'horizontal)
(define frame (new frame%
                   [label "Circuit Analyzer"]
                   [min-width 800]
                   [min-height 600]
                   [stretchable-height #f]
                   [stretchable-width #f]))




(define my-pasteboard%
  (class pasteboard%
    (define/augment (can-move-to? snip	x y dragging?)
      (if (equal? snip back) #f #t))
    (define/augment (can-select? snip on?)
      (if (equal? snip back) #f #t))
    (define/augment (on-delete snip)
      (let
          ((table-name (cond
                         ((eq? (cdr current-snip) 'resistor) resistor-gui-table)
                         ((eq? (cdr current-snip) 'battery) battery-gui-table)
                         ((eq? (cdr current-snip) 'earth) earth-gui-table)
                         ((eq? (cdr current-snip) 'wire) wire-gui-table)
                         ((eq? (cdr current-snip) 'voltmeter) volt-gui-table))))
        (if (is-a? snip image-snip%) (display "")
            (begin (vector-set! table-name (send snip get-index) #f)
                   (let
                       ((pos1 (send snip get-current-positions))
                        (pos2 (send snip get-current-positions1)))
                     (if (eq? pos1 'undefined)
                         (display "")
                         (begin
                           (2d-vector-set! board (car pos1) (cdr pos1) (remove snip (2d-vector-ref board (car pos1) (cdr pos1))))
                           (cond ((not (equal? pos2 'undefined)) 
                                  (2d-vector-set! board (car pos2) (cdr pos2) (remove snip (2d-vector-ref board (car pos2) (cdr pos2)))))))))))))    
    (define/augment (after-select snip on?)
      (remake-details-box snip))
    (define/augment (after-move-to snip	x y dragging?)
      (send snip update-pos x y))
    (super-new)))





(define menu-bar-top (new menu-bar% [parent frame]))

(define file-menu (new menu% [label "&File"] [parent menu-bar-top]))
(define new-file (new menu-item% [label "&New"] [parent file-menu] [callback (λ (x y) (erase-all))]))
(define open-file (new menu-item% [label "&Open"] [parent file-menu] [callback (λ (x y) (void))]))
(define save-file (new menu-item% [label "&Save"] [parent file-menu] [callback (λ (x y) (void))]))
(define save-as-file (new menu-item% [label "Save &As"] [parent file-menu] [callback (λ (x y) (void))]))
(define quit (new menu-item% [label "&Quit"] [parent file-menu] [callback (λ (x y) (exit))]))

(define edit-menu (new menu% [label "&Edit"] [parent menu-bar-top]))
(define copy-item (new menu-item% [label "&Copy"] [parent edit-menu] [callback (λ (x y) (void))]))
(define paste-item (new menu-item% [label "&Paste"] [parent edit-menu] [callback (λ (x y) (void))]))
;(define paste-item (new menu-item% [label "&Paste"] [parent edit-menu] [callback (λ (x y) (void))]))

(define help-menu (new menu% [label "&Help"] [parent menu-bar-top]))
(define help-topics (new menu-item% [label "&Index"] [parent help-menu] [callback (λ (x y) (begin
                                                                                             (send help-window show #t)
                                                                                             (begin (sleep/yield 0.1) (send ec-dc draw-bitmap image-i 0 0))))]))
(define about (new menu-item% [label "&About Us"] [parent help-menu] [callback (λ (x y) (begin
                                                                                          (send about-us show #t)
                                                                                          (begin (sleep/yield 0.1) (send ec-dc1 draw-bitmap image-au1 0 0))))]))


(define vertical-pane-tools (new vertical-pane% [parent frame]))
(define horizontal-pane1 (new horizontal-pane%
                              [parent vertical-pane-tools]
                              [border 10]
                              ))
(define horizontal-pane4 (new horizontal-pane%
                              [parent horizontal-pane1]
                              [min-width 720]
                              ))
(define wire-button (new button%
                         [label (read-bitmap "wire.png")]
                         [min-width 100]
                         [min-height 50]
                         [parent horizontal-pane4]
                         [callback (lambda (b e) (make-object wire-gui% pb))]))
(define resistor-button (new button%
                             [label (read-bitmap "resistance.png")]
                             [min-width 100]
                             [min-height 50]
                             [parent horizontal-pane4]
                             [callback (lambda (b e) (make-object resistor-gui% pb))]))
(define battery-button (new button%
                            [label (read-bitmap "battery.png")]
                            [min-width 50]	 
                            [min-height 50]
                            [parent horizontal-pane4]
                            [callback (lambda (b e) (make-object battery-gui% pb))]))
(define earth-button (new button%
                          [label (read-bitmap "earth2.png")]
                          [min-width 50]	 
                          [min-height 50]
                          [parent horizontal-pane4]
                          [callback (lambda (b e) (make-object earth-gui% pb))]))
(define volt-button (new button%
                         [label (read-bitmap "voltmeter2.png")]
                         [min-width 50]	 
                         [min-height 50]
                         [parent horizontal-pane4]
                         [callback (lambda (b e) (make-object volt-gui% pb))]))

;(define capacitor-button (new button%
;                              [label (read-bitmap "capacitor.png")]
;                              [min-width 50]	 
;                              [min-height 50]
;                              [parent horizontal-pane4]))

(define horizontal-pane3 (new horizontal-pane%
                              [parent horizontal-pane1]
                              ))
(define play-button (new button%
                         [label (read-bitmap "play.png")]
                         [min-width 50]	 
                         [min-height 50]
                         [parent horizontal-pane3]
                         [callback (lambda (b e) (begin
                                                   (make-circuit)
                                                   (execute)))]))


(define horizontal-pane2 (new horizontal-pane%
                              [parent vertical-pane-tools]))

(define pb (new my-pasteboard%))
(define ec-pb (new editor-canvas%
                   [parent horizontal-pane2]
                   [editor pb]
                   [label "abc"]
                   [min-height 575]
                   [min-width 625]
                   [style (list 'no-hscroll 'no-vscroll)]))

(define vertical-pane-dialogues (new vertical-pane% [parent horizontal-pane2]))

(define title-face (make-object font% 14 'default 'normal 'bold	#t))
(define details-box (new vertical-pane% [parent vertical-pane-dialogues] [border 5] [min-height 300]))
(define component-name (new message% [parent details-box] [label "None Selected"] [font title-face]))
(define vertical? (new check-box% [label "Vertical"] [parent details-box] [callback (λ (i e) (callback1 'chk))] [enabled #f]))
(define reverse? (new check-box% [label "Reverse"] [parent details-box] [callback (λ (i e) (callback1 'chk))] [enabled #f]))
(define value1 (new text-field% [label "value-name123 :"] [parent details-box] [init-value "#f"] [enabled #f]))
(define value2 (new text-field% [label "value-name456 :"] [parent details-box] [init-value "#f"] [enabled #f]))
(define set-values (new button% [label "Set!"] [parent details-box] [callback (λ (i e) (callback1 'set))] [enabled #f]))

(define passive-box (new vertical-pane% [parent vertical-pane-dialogues] [border 5]))
(define value3 (new text-field% [label "value-name123 :"] [parent passive-box] [init-value "#f"] [enabled #f]))
(define value4 (new text-field% [label "value-name123 :"] [parent passive-box] [init-value "#f"] [enabled #f]))
(define current-snip (void))
(define (remake-details-box snip)
  (begin
    (cond
      ((eq? (send snip get-type) 'voltmeter)
       (send value3 set-label (send snip get-name-of-value1))
       (send value3 set-value (number->string (if (send snip get-value1) (send snip get-value1) 0)))
       (send value4 set-label "..")
       (send value4 set-value "..")))
    (cond
      ((or (eq? (send snip get-type) 'battery)
           (eq? (send snip get-type) 'resistor))
       (send value3 set-label "..")
       (send value3 set-value "..")
       (send value4 set-label "Current :")
       (send value4 set-value (number->string (if (send snip get-current) (send snip get-current) 0)))))
    (begin
      (cond 
        ((= (send snip get-no-of-values) 1)
         (begin
           (send value1 enable #t)
           (send value1 set-label (send snip get-name-of-value1))
           (send value1 set-value (number->string (if (send snip get-value1) (send snip get-value1) 0)))
           (send value2 enable #f)
           (send value2 set-label "no value")
           (send value2 set-value "no value")
           (send vertical? enable #t)
           (send vertical? set-value (eq? (send snip get-orientation) 'v))
           (send set-values enable #t)
           (set! current-snip (cons (send snip get-index) (send snip get-type)))))
        ((= (send snip get-no-of-values) 2) 
         (begin
           (send value1 enable #t)
           (send value1 set-label (send snip get-name-of-value1))
           (send value1 set-value (number->string (if (send snip get-value1) (send snip get-value1) 0)))
           (send value2 enable #t)
           (send value2 set-label (send snip get-name-of-value2))
           (send value2 set-value (number->string (if (send snip get-value2) (send snip get-value2) 0)))
           (send vertical? enable #t)
           (send vertical? set-value (eq? (send snip get-orientation) 'v))
           (send set-values enable #t)
           (set! current-snip (cons (send snip get-index) (send snip get-type)))))
        (else 
         (begin
           (send value1 enable #f)
           (send value1 set-label "no-value")
           (send value1 set-value "no-value")
           (send value2 enable #f)
           (send value2 set-label "no-value")
           (send value2 set-value "no-value")
           (send vertical? enable #t)
           (send vertical? set-value (eq? (send snip get-orientation) 'v))
           (send set-values enable #f)
           (set! current-snip (cons (send snip get-index) (send snip get-type))))))
      (send component-name set-label (send snip get-own-name)))))
(define (callback1 id)
  (let
      ((table-name (cond
                     ((eq? (cdr current-snip) 'resistor) resistor-gui-table)
                     ((eq? (cdr current-snip) 'battery) battery-gui-table)
                     ((eq? (cdr current-snip) 'earth) earth-gui-table)
                     ((eq? (cdr current-snip) 'wire) wire-gui-table)
                     ((eq? (cdr current-snip) 'voltmeter) volt-gui-table))))
    (cond
      ((eq? id 'chk) (send (vector-ref table-name (car current-snip)) change-orientation))
      ((eq? id 'set) (begin
                       (send (vector-ref table-name (car current-snip))
                             set-values
                             (string->number (send value1 get-value))
                             (string->number (send value2 get-value))))))))

(define corr-pos 'undefined)
(define (make-circuit)  
  (reset-resistor-vec)
  (reset-battery-vec)
  (reset-node-vec)
  ;(reset-current-source-vec)
  (reset-wire-list)
  (set! corr-pos (make-nodes board))
  (make-resistor-vec resistor-gui-table corr-pos)
  (make-battery-vec battery-gui-table corr-pos)
  (make-earth-vec earth-gui-table corr-pos)
  (make-wire-vec wire-gui-table corr-pos)
  )


(define G 'undefined)
(define BC 'undefined)
(define B 'undefined)
(define C 'undefined)
(define D 'undefined)
(define A 'undefined)
(define Z 'undefined)
(define X 'undefined)
(define (execute)
  (evaluate-all-wires)
  (set! G (make-G-matrix))
  (set! BC (make-BC-matrix))
  (set! B (car BC))
  (set! C (cdr BC))
  (set! D (make-D-matrix))
  (set! A (make-A-matrix G C B D))
  (set! Z (make-Z-matrix))
  (set! X (solve-matrix A Z))
  (fill-values X)
  (make-volt volt-gui-table corr-pos)
  (fill-currents-gui battery-gui-table resistor-gui-table)
  )

(define (erase-all)
  (begin
    (reset-resistor-vec)
    (reset-battery-vec)
    (reset-node-vec)
    (reset-wire-list)
    (send pb erase)
    (reset-gui)
    (send pb insert back 0 0)))

(define back (make-object image-snip% (read-bitmap "back.png")))
(send pb insert back 0 0)
(send frame show #t)




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
       [callback (lambda (m x) (begin (send help-window show #f)
                                      (send frame show #t)
                                      ))]))


(define about-us (new frame%
                      [label "About Us"]
                      [min-height 415]
                      [min-width 345]
                      [stretchable-width #f]
                      [stretchable-height #f]))
(define ec1 (new canvas% 
                 [parent about-us]
                 [horiz-margin 5]
                 [vert-margin 5]))
(define ec-dc1 (send ec1 get-dc))
(define image-au1 (read-bitmap "aboutus1.png" `png))

(define close-button1
  (new button%
       [label "Back"]
       [parent about-us]
       [callback (lambda (m x) (begin (send about-us show #f)))]))




