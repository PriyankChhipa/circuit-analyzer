#lang racket

(require racket/mpair)
(provide earth%                  make-current-source    current-source%    voltmeter%
         make-2d-vector          2d-vector-ref          2d-vector-set!     ammeter%
         resistor-vec            battery-vec            node-vec battery-real%
         no-of-resistors         no-of-batteries        no-of-nodes
         make-resistor           make-battery           make-node evaluate-all-wires wire%
         make-wire delete-wire
         reset-resistor-vec reset-battery-vec reset-node-vec reset-wire-list
         voltmeter% is-in?)



(define (make-2d-vector r c ini)
  (build-vector r 
                (lambda (x) (make-vector c ini))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

(define (is-in? elem lst)
  (cond
    ((null? lst) #f)
    ((equal? elem (car lst)) #t)
    (else (is-in? elem (cdr lst)))))


;objects are defined here as elements of vectors
(define resistor-vec (make-vector 100 #f))
(define battery-vec (make-vector 100 #f))
(define node-vec (make-vector 200 #f))
(define current-source-vec (make-vector 100 #f))
(define wire-list '())

;number of objects of each type
(define no-of-resistors 0)
(define no-of-batteries 0)
(define no-of-nodes 0)
(define no-of-current-sources 0)

(define (reset-resistor-vec)
  (set! resistor-vec (make-vector 100 #f))
  (set! no-of-resistors 0))

(define (reset-battery-vec)
  (set! battery-vec (make-vector 100 #f))
  (set! no-of-batteries 0))

(define (reset-node-vec)
  (set! node-vec (make-vector 200 #f))
  (set! no-of-nodes 0))

(define (reset-wire-list)
  (set! wire-list '()))

;creation of objects
(define (make-resistor node1 node2 resistance)
  (vector-set! resistor-vec no-of-resistors (make-object resistor% node1 node2 resistance))
  (set! no-of-resistors (+ no-of-resistors 1)))

(define (make-battery node1 node2 voltage-rating . resistance)
  (if (null? resistance)
      (begin
        (vector-set! battery-vec no-of-batteries (make-object battery-ideal% node1 node2 voltage-rating))
        (set! no-of-batteries (+ no-of-batteries 1)))
      (make-object battery-real% node1 node2 voltage-rating (car resistance))))


(define (make-node)
  (vector-set! node-vec no-of-nodes (make-object node%))
  (set! no-of-nodes (+ no-of-nodes 1)))

(define (make-current-source node1 current-value)
  (vector-set! current-source-vec no-of-current-sources (make-object current-source% node1 current-value))      
  (set! no-of-current-sources (+ no-of-current-sources 1)))

(define (make-wire node1 node2)
  (set! wire-list (append wire-list (list (make-object wire% node1 node2)))))

(define (delete-wire wire)
  (set! wire-list (remove wire wire-list)))

(define (next-empty-pos wire-vec)
  (define (assist i)
    (cond
      ((not (vector-ref wire-vec i)) i)
      (else (assist (+ i 1)))))
  (assist 0))

;resistor class
(define resistor%
  (class object%
    ;initializations
    (init-field node1)
    (init-field node2)
    (init-field [resistance 0])
    (super-new)
    
    ;definitions
    (define current #f)
    (define/public (get-node1) node1)
    (define/public (get-node2) node2)
    (define/public (get-resistance) resistance)
    (define/public (get-current) current)
    
    (define/public (has-resistance?) #t)
    
    (define/public (set-current c) (set! current c))
    
    ;constructors
    (send node1 connect this)
    (send node2 connect this)))


;node class
(define node%
  (class object%
    ;initializations
    (init-field [voltage #f])
    (init-field [dependent #f])
    (init-field [parent-node #f])
    (super-new)
    
    ;definitions
    
    (define connected-things (list))
    (define connected-wires (list))
    (define/public (connect thing)
      (set! connected-things (cons thing connected-things)))
    (define/public (connect-wire wire)
      (set! connected-wires (cons wire connected-wires)))
    (define/public (disconnect wire)
      (set! connected-wires (remove wire connected-wires)))
    (define/public (get-voltage) voltage)
    (define/public (make-dependent) (set! dependent #t))
    (define/public (get-connected-things) connected-things)
    (define/public (get-connected-wires) connected-wires)
    
    (define/public (set-voltage v) (set! voltage v))))


;battery class
(define battery-ideal%
  (class object%
    ;initializations
    (init-field node1)
    (init-field node2)
    (init-field voltage-difference)
    (super-new)
    
    ;definitions
    (define pseudo-node1 #f)
    (define pseudo-node2 #f)
    
    (define current #f)
    (define/public (get-EMF) voltage-difference)
    (define/public (get-current) current)
    (define/public (get-node1) (if pseudo-node1 pseudo-node1 node1))
    (define/public (get-node2) (if pseudo-node2 pseudo-node2 node2))
    (define/public (change-pseudo-node node nnode)
      (if (or (equal? node node1) (equal? node pseudo-node1))
          (set! pseudo-node1 nnode)
          (set! pseudo-node2 nnode)))
    (define/public (has-resistance?) #f)
    
    (define/public (set-current c) (set! current c))
    
    ;constructors
    (send node1 connect this)
    (send node2 connect this)))


(define battery-real%
  (class object%
    (init-field node1)
    (init-field node2)
    (init-field voltage-difference)
    (init-field resistance)
    (super-new)
    
    (define pseudo-node1 #f)
    (define pseudo-node2 #f)
    (define internal-node (begin (make-node) (vector-ref node-vec (- no-of-nodes 1))))
    (make-resistor internal-node node2 resistance)
    (define internal-ideal-battery (begin (make-battery node1 internal-node voltage-difference) (vector-ref battery-vec (- no-of-batteries 1))))
    
    (define/public (get-EMF) voltage-difference)
    (define/public (get-current) (send internal-ideal-battery get-current))
    (define/public (get-node1) (if pseudo-node1 pseudo-node1 node1))
    
    (define/public (change-pseudo-node node nnode)      
      (if (or (equal? node node1) (equal? node pseudo-node1))
          (begin (send internal-ideal-battery change-pseudo-node node nnode) (set! pseudo-node1 nnode))
          (set! pseudo-node2 nnode)))
    
    (define/public (get-internal-node) internal-node)
    (define/public (get-node2) (if pseudo-node2 pseudo-node2 node2))
    
    (define/public (has-resistance?) #f)
    
    (define/public (set-current c) (send internal-ideal-battery set-current c))))

;earth class
(define earth%
  (class object%
    ;initializations
    (init-field node1)
    (super-new)
    
    ;definitions
    (define/public (has-resistance?) #f)
    
    ;constructors
    (send node1 connect this)
    (send node1 set-voltage 0)))

;current source class
(define current-source%
  (class object%
    ;initializations
    (init-field node1)
    (init-field current-value)
    (super-new)
    
    ;definitions
    (define/public (get-node1) node1)
    (define/public (get-current) current-value)
    (define/public (has-resistance?) #f)
    
    ;constructors
    (send node1 connect this)     
    ))

;wire class
;(define wire%
;  (class object%
;    
;    (super-new)

;ammeter class
(define ammeter%
  (class object%
    ;initializations
    (init-field object)
    (super-new)
    
    (if (is-a? object resistor%)
        (display "Current in resistor = ")
        (display "Current in battery = "))
    (display (send object get-current))
    (newline)))

;voltmeter class
(define voltmeter%
  (class object%
    ;initializations
    (init-field node1)
    (init-field node2)
    (init-field [vd #f])
    (super-new)
    
    (set! vd (- (send node1 get-voltage) (send node2 get-voltage)))))

;wire class
(define wire%
  (class object%
    ;initializations
    (init-field node1)
    (init-field node2)
    (super-new)
    
    (define/public (get-other-node node) (if (equal? node node1) node2 node1))
    (define/public (move-things node2 node1)
      (define things (send node2 get-connected-things))
      (define already-exist (send node1 get-connected-things))
      (define (assist lst)
        (cond
          ((null? lst) (display ""))
          (else
           (begin
             (if (is-in? (car lst) already-exist) 
                 (display "")
                 (send node1 connect (car lst)))
             (if (is-a? (car lst) battery-ideal%)
                 (send (car lst) change-pseudo-node node2 node1)
                 (display ""))
             (assist (cdr lst))))))
      (assist things))
    (define/public (make-wires-new node2 node1)
      (define wires (send node2 get-connected-wires))
      (define (assist lst)
        (cond
          ((null? lst) (display ""))
          (else
           (begin
             (let
                 ((mnode (send (car lst) get-other-node node2)))
               (if (equal? mnode node1) (assist (cdr lst))
                   (begin (make-wire node1 mnode)
                          (assist (cdr lst)))))))))
      (assist wires))
    (define/public (delete-wires node2)
      (define wires (send node2 get-connected-wires))
      (define (assist lst)
        (cond
          ((null? lst) (display ""))
          (else
           (begin
             (send node2 disconnect (car lst))
             (delete-wire (car lst))
             (assist (cdr lst))))))
      (assist wires))
    (define/public (evaluate!)
      (move-things node2 node1)
      (make-wires-new node2 node1)
      (send node2 make-dependent)
      (if (send node2 get-voltage) (send node1 set-voltage (send node2 get-voltage)) (display ""))
      (delete-wires node2)
      (set-field! parent-node node2 node1))
    
    (send node1 connect-wire this)
    (send node2 connect-wire this)))

(define (evaluate-all-wires)
  (cond
    ((null? wire-list) 'done)
    (else
     (begin
       (send (car wire-list) evaluate!)
       (delete-wire wire-list)
       (evaluate-all-wires)))))