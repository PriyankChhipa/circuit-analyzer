#lang racket
(require racket/gui)
(require "project.rkt")
(provide resistor-gui%
         resistor-gui-table
         resistor-gui-index
         battery-gui%
         battery-gui-table
         battery-gui-index
         earth-gui%
         earth-gui-table
         earth-gui-index
         wire-gui%
         wire-gui-table
         wire-gui-index
         volt-gui%
         volt-gui-table
         volt-gui-index
         board
         reset-gui)

(define (reset-gui)
  (set! resistor-gui-table (make-vector 100 #f))
  (set! resistor-gui-index 0)
  (set! battery-gui-table (make-vector 100 #f))
  (set! battery-gui-index 0)
  (set! earth-gui-table (make-vector 100 #f))
  (set! earth-gui-index 0)
  (set! wire-gui-table (make-vector 200 #f))
  (set! wire-gui-index 0)
  (set! volt-gui-table (make-vector 100 #f))
  (set! volt-gui-index 0)
  (set! board (make-2d-vector 15 15 '())))

(define board (make-2d-vector 15 15 '()))

(define (alter-pos pos o)
  (if (eq? o 'h) (cons (car pos) (+ 14 (cdr pos)))
      (cons (+ 14 (car pos)) (cdr pos))))

(define (near? point)
  (let
      ((rem (remainder point 56)))
    (or (< (abs (- rem 56)) 8) (< (abs rem) 8))))

(define (nearest-multiple n)
  (if (> (remainder n 56) 28) (* (+ (quotient n 56) 1) 56) (* (quotient n 56) 56)))

(define (get-point-on-board point)
  (cons (inexact->exact (/ (nearest-multiple (car point)) 56)) (inexact->exact (/ (nearest-multiple (cdr point)) 56)))) 

(define resistor-gui-table (make-vector 100 #f))

(define resistor-gui-index 0)

(define resistor-snip%
  (class image-snip%
    (define index resistor-gui-index)
    (define resistance #f)
    (define current #f)
    (define resistor #f)
    (define current-positions 'undefined)
    (define current-positions1 'undefined)
    (define orientation 'h)
    (super-new)
    
    (define (reset) (if (eq? orientation 'v) (send this set-bitmap (read-bitmap "resistance2.png"))
                        (send this set-bitmap (read-bitmap "resistance1.png"))))
    (define/public (get-index) index)
    (define/public (get-value1) resistance)
    (define/public (get-no-of-values) 1)
    (define/public (get-name-of-value1) "Resistance :")
    (define/public (get-own-name) (string-append "Resistance " (string (integer->char (+ index 65)))))
    (define/public (get-type) 'resistor)
    (define/public (get-orientation) orientation)
    (define/public (get-current-positions) current-positions)
    (define/public (get-current-positions1) current-positions1)
    (define/public (set-values r dummy) (set! resistance r))
    (define/public (get-current) current)
    (define/public (connect r) (set! resistor r))
    (define/public (update-value) (set! current (send resistor get-current)))
    (define/public (set-current c) (set! current c))
    (define/public (set-on-board x y) (void))
    (define/public (change-orientation) (begin (if (eq? orientation 'h) (set! orientation 'v) (set! orientation 'h))
                                               (reset)))
    
    (define/public (update-pos x y)
      (let
          ((actual-pos (alter-pos (cons x y) orientation)))
        (if (and (near? (car actual-pos)) (near? (cdr actual-pos)))
            (begin
              (if (eq? current-positions 'undefined)
                  (begin
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions)) ))
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))))
            (begin
              (if (eq? current-positions 'undefined) (display "")
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions))))
                    (set! current-positions 'undefined)
                    (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                    (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                    (set! current-positions1 'undefined)))))))))

(define resistor-gui%
  (class object%
    (init-field pb)
    (super-new)
    ;(define resistor-snip (make-object resistor-snip% (read-bitmap "resistance1.png")))
    (vector-set! resistor-gui-table resistor-gui-index (make-object resistor-snip% (read-bitmap "resistance1.png")))
    
    (send pb insert (vector-ref resistor-gui-table resistor-gui-index) 56 42)
    (send pb set-before (vector-ref resistor-gui-table resistor-gui-index) #f)
    (set! resistor-gui-index (+ resistor-gui-index 1))))

(define battery-gui-table (make-vector 100 #f))
(define battery-gui-index 0)
(define battery-snip%
  (class image-snip%
    (define index battery-gui-index)
    (define resistance 0)
    (define emf #f)
    (define current #f)
    (define battery #f)
    (define current-positions 'undefined)
    (define current-positions1 'undefined)
    (define orientation 'h)
    (super-new)
    
    (define (reset) (if (eq? orientation 'v) (send this set-bitmap (read-bitmap "battery2.png"))
                        (send this set-bitmap (read-bitmap "battery1.png"))))
    
    (define/public (get-index) index)
    (define/public (get-no-of-values) 2)
    (define/public (get-name-of-value1) "EMF :")
    (define/public (get-value1) emf)
    (define/public (get-name-of-value2) "Resistance :")
    (define/public (get-value2) resistance)
    (define/public (get-orientation) orientation)
    (define/public (get-current) current)
    (define/public (set-current c) (set! current c))
    (define/public (get-current-positions) current-positions)
    (define/public (get-current-positions1) current-positions1)
    (define/public (get-own-name) (string-append "Battery " (string (integer->char (+ index 65)))))
    (define/public (get-type) 'battery)
    (define/public (connect b) (set! battery b))
    (define/public (update-value) (set! current (send battery get-current)))
    (define/public (set-values e r) (set! emf e) (set! resistance r))
    (define/public (change-orientation) (begin (if (eq? orientation 'h) (set! orientation 'v) (set! orientation 'h))
                                               (reset)))
    (define/public (update-pos x y)
      (let
          ((actual-pos (alter-pos (cons x y) orientation)))
        (if (and (near? (car actual-pos)) (near? (cdr actual-pos)))
            (begin
              (if (eq? current-positions 'undefined)
                  (begin
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions)) ))
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))))
            (begin
              (if (eq? current-positions 'undefined) (display "")
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions))))
                    (set! current-positions 'undefined)
                    (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                    (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                    (set! current-positions1 'undefined)))))))))
(define battery-gui%
  (class object%
    (init-field pb)
    (super-new)
    ;(define battery-snip (make-object battery-snip% (read-bitmap "battery1.png")))
    (vector-set! battery-gui-table battery-gui-index (make-object battery-snip% (read-bitmap "battery1.png")))
    
    (send pb insert (vector-ref battery-gui-table battery-gui-index) 56 42)
    (send pb set-before (vector-ref battery-gui-table battery-gui-index) #f)
    (set! battery-gui-index (+ battery-gui-index 1))))

(define earth-gui-table (make-vector 100 #f))
(define earth-gui-index 0)
(define earth-snip%
  (class image-snip%
    (define index earth-gui-index)
    (define orientation 'h)
    (define current-positions 'undefined)
    (define current-positions1 'undefined)
    (super-new)
    
    (define (reset) (if (eq? orientation 'v) (send this set-bitmap (read-bitmap "earth1.png"))
                        (send this set-bitmap (read-bitmap "earth.png"))))
    
    
    (define/public (get-no-of-values) 0)
    (define/public (get-index) index)
    (define/public (get-type) 'earth)  
    (define/public (get-orientation) orientation)
    (define/public (get-current-positions) current-positions)
    (define/public (get-current-positions1) current-positions1)
    (define/public (get-own-name) (string-append "Earth " (string (integer->char (+ index 65)))))
    (define/public (change-orientation) (begin (if (eq? orientation 'h) (set! orientation 'v) (set! orientation 'h))
                                               (reset)))
    (define/public (update-pos x y)
      (let
          ((actual-pos (alter-pos (cons x y) orientation)))
        (if (and (near? (car actual-pos)) (near? (cdr actual-pos)))
            (begin
              (if (eq? current-positions 'undefined)
                  (begin
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this))))
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions)) ))
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this))))))
            (begin
              (if (eq? current-positions 'undefined) (display "")
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions))))
                    (set! current-positions 'undefined)))))))))
(define earth-gui%
  (class object%
    (init-field pb)
    (super-new)
    ;(define earth-snip (make-object earth-snip% (read-bitmap "earth.png")))
    (vector-set! earth-gui-table earth-gui-index (make-object earth-snip% (read-bitmap "earth.png")))
    
    (send pb insert (vector-ref earth-gui-table earth-gui-index) 56 42)
    (send pb set-before (vector-ref earth-gui-table earth-gui-index) #f)
    (set! earth-gui-index (+ earth-gui-index 1))))

(define wire-gui-table (make-vector 100 #f))
(define wire-gui-index 0)
(define wire-snip%
  (class image-snip%
    (define index wire-gui-index)
    (define orientation 'h)
    (define current-positions 'undefined)
    (define current-positions1 'undefined)
    (super-new)
    
    (define (reset) (if (eq? orientation 'v) (send this set-bitmap (read-bitmap "wire1.png"))
                        (send this set-bitmap (read-bitmap "wire.png"))))
    
    
    (define/public (get-no-of-values) 0)
    (define/public (get-index) index)
    (define/public (get-type) 'wire)  
    (define/public (get-orientation) orientation)
    (define/public (get-current-positions) current-positions)
    (define/public (get-current-positions1) current-positions1)
    (define/public (get-own-name) (string-append "Wire " (string (integer->char (+ index 65)))))
    (define/public (change-orientation) (begin (if (eq? orientation 'h) (set! orientation 'v) (set! orientation 'h))
                                               (reset)))
    (define/public (update-pos x y)
      (let
          ((actual-pos (alter-pos (cons x y) orientation)))
        (if (and (near? (car actual-pos)) (near? (cdr actual-pos)))
            (begin
              (if (eq? current-positions 'undefined)
                  (begin
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions)) ))
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))))
            (begin
              (if (eq? current-positions 'undefined) (display "")
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions))))
                    (set! current-positions 'undefined)
                    (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                    (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                    (set! current-positions1 'undefined)))))))))
    
(define wire-gui%
  (class object%
    (init-field pb)
    (super-new)
    ;(define earth-snip (make-object earth-snip% (read-bitmap "earth.png")))
    (vector-set! wire-gui-table wire-gui-index (make-object wire-snip% (read-bitmap "wire.png")))
    
    (send pb insert (vector-ref wire-gui-table wire-gui-index) 56 42)
    (send pb set-before (vector-ref wire-gui-table wire-gui-index) #f)
    (set! wire-gui-index (+ wire-gui-index 1))))


(define volt-gui-table (make-vector 100 #f))
(define volt-gui-index 0)
(define volt-snip%
  (class image-snip%
    (define index volt-gui-index)
    (define voltage 0)
    (define current-positions 'undefined)
    (define current-positions1 'undefined)
    (define orientation 'h)
    (define v-meter 'none)
    (super-new)
    
    (define (reset) (if (eq? orientation 'v) (send this set-bitmap (read-bitmap "voltmeter1.png"))
                        (send this set-bitmap (read-bitmap "voltmeter.png"))))
    
    (define/public (get-index) index)
    (define/public (get-no-of-values) 0)
    (define/public (get-name-of-value1) "Voltage :")
    (define/public (get-value1) voltage)
    (define/public (get-orientation) orientation)
    (define/public (get-current-positions) current-positions)
    (define/public (get-current-positions1) current-positions1)
    (define/public (get-own-name) (string-append "Voltmeter " (string (integer->char (+ index 65)))))
    (define/public (get-type) 'voltmeter)
    (define/public (set-values v r) (set! voltage v))
    (define/public (change-orientation) (begin (if (eq? orientation 'h) (set! orientation 'v) (set! orientation 'h))
                                               (reset)))
    (define/public (connect v)
      (set! v-meter v))
    (define/public (update-value)
      (set! voltage (get-field vd v-meter)))
    (define/public (update-pos x y)
      (let
          ((actual-pos (alter-pos (cons x y) orientation)))
        (if (and (near? (car actual-pos)) (near? (cdr actual-pos)))
            (begin
              (if (eq? current-positions 'undefined)
                  (begin
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions)) ))
                    (set! current-positions (get-point-on-board actual-pos))
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (append (2d-vector-ref board  (car current-positions) (cdr current-positions)) (list this)))
                    (if (eq? orientation 'h)
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (+ (car current-positions) 1) (cdr current-positions)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))
                        (begin
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                          (set! current-positions1 (cons (car current-positions) (+ (cdr current-positions) 1)))
                          (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                          (append (2d-vector-ref board  (car current-positions1) (cdr current-positions1)) (list this))))))))
            (begin
              (if (eq? current-positions 'undefined) (display "")
                  (begin
                    (2d-vector-set! board  (car current-positions) (cdr current-positions)
                                    (remove this (2d-vector-ref board  (car current-positions) (cdr current-positions))))
                    (set! current-positions 'undefined)
                    (2d-vector-set! board  (car current-positions1) (cdr current-positions1)
                                    (remove this (2d-vector-ref board  (car current-positions1) (cdr current-positions1))))
                    (set! current-positions1 'undefined)))))))))
(define volt-gui%
  (class object%
    (init-field pb)
    (super-new)
    ;(define battery-snip (make-object battery-snip% (read-bitmap "battery1.png")))
    (vector-set! volt-gui-table volt-gui-index (make-object volt-snip% (read-bitmap "voltmeter.png")))
    
    (send pb insert (vector-ref volt-gui-table volt-gui-index) 56 42)
    (send pb set-before (vector-ref volt-gui-table volt-gui-index) #f)
    (set! volt-gui-index (+ volt-gui-index 1))))



    