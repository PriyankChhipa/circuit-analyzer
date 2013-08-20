#lang racket

(require "project.rkt")
(provide make-nodes
         make-resistor-vec
         make-battery-vec
         make-earth-vec
         make-wire-vec
         make-volt
         fill-currents-gui)

(define (make-nodes board)
  (define i (vector-length board))
  (define j (vector-length (vector-ref board 0)))
  (define corresponding-positions '())
  (define (assist_i i)
    (define (assist_j j)
      (cond
        ((not (< j 15)) (assist_i (+ i 1)))
        ((null? (2d-vector-ref board i j)) (assist_j (+ j 1)))
        (else
         (begin
           (make-node)
           (set! corresponding-positions (append corresponding-positions (list (cons i j))))
           (assist_j (+ j 1))))))
    (cond
      ((< i 15) (assist_j 0))))
  (begin
    (assist_i 0)
    corresponding-positions))

(define (make-resistor-vec resistor-gui-table corr-pos)
  (define (assist i)
    (cond
      ((not (< i 100)) (display 'r-done))
      ((if (vector-ref resistor-gui-table i) #t #f)
       (begin
         (make-resistor (vector-ref node-vec (- (length corr-pos) 
                                                (length (member (send (vector-ref resistor-gui-table i) get-current-positions) corr-pos))))
                        (vector-ref node-vec (- (length corr-pos)
                                                (length (member (send (vector-ref resistor-gui-table i) get-current-positions1) corr-pos))))
                        (send (vector-ref resistor-gui-table i) get-value1))
         (send (vector-ref resistor-gui-table i) connect (vector-ref resistor-vec (- no-of-resistors 1)))
         (assist (+ i 1))))
      (else (assist (+ i 1)))))
  (assist 0))

(define (make-battery-vec battery-gui-table corr-pos)
  (define (assist i)
    (cond
      ((not (< i 100)) (display 'b-done))
      ((if (vector-ref battery-gui-table i) #t #f)
       (begin
         (if (= (send (vector-ref battery-gui-table i) get-value2) 0)
             (begin
               (make-battery (vector-ref node-vec (- (length corr-pos) 
                                                   (length (member (send (vector-ref battery-gui-table i) get-current-positions) corr-pos))))
                           (vector-ref node-vec (- (length corr-pos)
                                                   (length (member (send (vector-ref battery-gui-table i) get-current-positions1) corr-pos))))
                           (send (vector-ref battery-gui-table i) get-value1))
               (send (vector-ref battery-gui-table i) connect (vector-ref battery-vec (- no-of-batteries 1))))
             (begin
             (make-battery (vector-ref node-vec (- (length corr-pos) 
                                                   (length (member (send (vector-ref battery-gui-table i) get-current-positions) corr-pos))))
                           (vector-ref node-vec (- (length corr-pos)
                                                   (length (member (send (vector-ref battery-gui-table i) get-current-positions1) corr-pos))))
                           (send (vector-ref battery-gui-table i) get-value1)
                           (send (vector-ref battery-gui-table i) get-value2))
             (send (vector-ref battery-gui-table i) connect (vector-ref battery-vec (- no-of-batteries 1)))))
         (assist (+ i 1))))
      (else (assist (+ i 1)))))
  (assist 0))

(define (make-earth-vec earth-gui-table corr-pos)
  (define (assist i)
    (cond
      ((not (< i 100)) (display 'e-done))
      ((if (vector-ref earth-gui-table i) #t #f)
       (begin
         (make-object earth% (vector-ref node-vec (- (length corr-pos) 
                                                (length (member (send (vector-ref earth-gui-table i) get-current-positions) corr-pos)))))
         (assist (+ i 1))))
      (else (assist (+ i 1)))))
  (assist 0))

(define (make-wire-vec wire-gui-table corr-pos)
  (define (assist i)
    (cond
      ((not (< i 100)) (display 'w-done))
      ((if (vector-ref wire-gui-table i) #t #f)
       (begin
         (make-wire (vector-ref node-vec (- (length corr-pos) 
                                                (length (member (send (vector-ref wire-gui-table i) get-current-positions) corr-pos))))
                        (vector-ref node-vec (- (length corr-pos)
                                                (length (member (send (vector-ref wire-gui-table i) get-current-positions1) corr-pos)))))
         (assist (+ i 1))))
      (else (assist (+ i 1)))))
  (assist 0))

(define (make-volt volt-gui-table corr-pos)
  (define (assist i)
    (cond
      ((not (< i 100)) (display 'v-done))
      ((if (vector-ref volt-gui-table i) #t #f)
       (begin
         (send (vector-ref volt-gui-table i) connect (make-object voltmeter% (vector-ref node-vec (- (length corr-pos) 
                                                (length (member (send (vector-ref volt-gui-table i) get-current-positions) corr-pos))))
                        (vector-ref node-vec (- (length corr-pos)
                                                (length (member (send (vector-ref volt-gui-table i) get-current-positions1) corr-pos))))))
         (send (vector-ref volt-gui-table i) update-value)
         (assist (+ i 1))))
      (else (assist (+ i 1)))))
  (assist 0))

(define (fill-currents-gui battery-gui-table resistor-gui-table)
  (define (assist i)
    (cond
      ((not (< i 100)) (display 'b-r-c-done))
      ((if (vector-ref battery-gui-table i) #t #f)
       (send (vector-ref battery-gui-table i) update-value)
       (assist (+ i 1)))
      (else (+ i 1))))
  (define (assist1 i)
    (cond
      ((not (< i 100)) (display 'b-r-c-done))
      ((if (vector-ref resistor-gui-table i) #t #f)
       (send (vector-ref resistor-gui-table i) update-value)
       (assist1 (+ i 1)))
      (else (+ i 1))))
  (begin
    (assist 0)
    (assist1 0)))