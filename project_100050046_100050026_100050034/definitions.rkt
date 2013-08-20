#lang racket

(require racket/mpair)
(provide earth%                  make-current-source    current-source%    voltmeter%
         make-2d-vector          2d-vector-ref          2d-vector-set!     ammeter%
         resistor-vec            battery-vec            node-vec           wire-list
         no-of-resistors         no-of-batteries        no-of-nodes        make-wire
         make-resistor           make-battery           make-node          propagate)


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
(define no-of-wires 0)

;creation of objects
(define (make-resistor node1 node2 resistance)
  (vector-set! resistor-vec no-of-resistors (make-object resistor% node1 node2 resistance))
  (set! no-of-resistors (+ no-of-resistors 1)))

(define (make-battery node1 node2 voltage-rating . resistance)
  (if (null? resistance)
      (begin
        (vector-set! battery-vec no-of-batteries (make-object battery-ideal% node1 node2 voltage-rating))
        (set! no-of-batteries (+ no-of-batteries 1)))
      (vector-set! battery-vec no-of-batteries (make-object battery-real% node1 node2 voltage-rating (car resistance)))))


(define (make-node)
  (vector-set! node-vec no-of-nodes (make-object node%))
  (set! no-of-nodes (+ no-of-nodes 1)))

(define (make-current-source node1 current-value)
  (vector-set! current-source-vec no-of-current-sources (make-object current-source% node1 current-value))      
  (set! no-of-current-sources (+ no-of-current-sources 1)))

(define (make-wire node1 node2)
  (set! wire-list (append wire-list (list (make-object wire% node1 node2)))))
  ;(set! no-of-wires (+ 1 no-of-wires))) 

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
    (define/public (get-node1) (if (get-field dependent? node1) (send node1 get-connected-nodes) (list node1)))
    (define/public (get-node2) (if (get-field dependent? node2) (send node2 get-connected-nodes) (list node2)))
    
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
    (init-field [dependent? #f])
    (super-new)
    
    ;definitions
    
    (define/public (dependency x)
      (set! dependent? x))
    ;connected items
    (define connected-wires (list))
    (define connected-things (list))
    (define connected-nodes (list))
    
    ;functions to edit the items connected
    (define/public (connect-nodes node)
      (set! connected-nodes (cons node connected-nodes)))
    (define/public (connect-wires wire)
      (set! connected-wires (cons wire connected-wires)))
    (define/public (connect thing)
      (set! connected-things (cons thing connected-things)))
    
    ;functions to get the connected items
    (define/public (get-voltage) voltage)
    (define/public (get-connected-things) connected-things)
    (define/public (get-connected-nodes) connected-nodes)
    (define/public (get-connected-wires) connected-wires)
    
    (define/public (set-voltage v) (if dependent? (set-dependent v connected-nodes) (set! voltage v)))
    (define (set-dependent v lst)
      (cond
        ((not (null? lst)) (begin
                             (send (car lst) set-voltage v)
                             (set-dependent v (cdr lst))))))))


;battery class
(define battery-ideal%
  (class object%
    ;initializations
    (init-field node1)
    (init-field node2)
    (init-field voltage-difference)
    (super-new)
    
    ;definitions
    (define current #f)
    
    (define/public (get-EMF) voltage-difference)
    (define/public (get-current) current)
    (define/public (get-node1) (if (get-field dependent? node1) (send node1 get-connected-nodes) (list node1)))
    (define/public (get-node2) (if (get-field dependent? node2) (send node2 get-connected-nodes) (list node2)))
    
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
    
    (define internal-node (begin (make-node) (vector-ref node-vec (- no-of-nodes 1))))
    (make-resistor internal-node node2 resistance)
    (define internal-ideal-battery (begin (make-battery node1 internal-node voltage-difference) (vector-ref battery-vec (- no-of-batteries 1))))
    
    (define/public (get-EMF) voltage-difference)
    (define/public (get-current) (send internal-ideal-battery get-current))
    (define/public (get-node1) (if (get-field dependent? node1) (send node1 get-connected-nodes) (list node1)))
    (define/public (get-node2) internal-node)
    
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
    (super-new)
    
    (display "Voltage = ")
    (display (- (send node1 get-voltage) (send node2 get-voltage)))
    (newline)))

;Wire Class
(define wire%
  (class object%
    ;initializations
    (init-field node1)
    (init-field node2)
    (super-new)
    
    (define/public (get-node1) node1)
    (define/public (get-node2) node2)
    (send node1 connect-wires this)
    (send node2 connect-wires this)
    (send node1 connect-nodes node2)
    (send node2 connect-nodes node1)
    
    (define/public (connectify lst)
      (cond
        ((not (null? lst)) 
         (begin
           (send node1 connect (car lst))
           (connectify (cdr lst))))))
    
    (define/public (connectify-wires lst)
      (cond
        ((not (null? lst)) 
         (begin
           (if (equal? (find-using node1 node2 lst) #f)
               '()
               (make-wire node1 (find-using node1 node2 lst)))
           (remove (car lst) wire-list)
           (connectify-wires (cdr lst))))))
    
    (define (find-using node1 node2 lst)
      (let ((node-h (if (equal? (send (car lst) get-node1) node2)
                        (send (car lst) get-node1)
                        (send (car lst) get-node1))))
        (if (equal? node-h node1) #f node-h)))
    
    (define/public (process)
      (define things (send node2 get-connected-things))
      (define wires (send node2 get-connected-wires))
      (begin
        (connectify things)
        (connectify-wires wires)        
        (send node2 set-voltage #t)
        (send node2 dependency #t)))     
    ))

(define (propagate lst)
  (cond
    ((not (null? lst))
     (begin (send (car lst) process)
            (propagate (cdr lst))))))


;(define (further-wires lst)
;(cond
; ((not (null? (cdr lst))) (begin
;                           (map (λ(x) (make-object wire% (car lst) x)) (cdr lst))
;                          (further-wires  (cdr lst))))))

; (cond 
; ((and (get-field dependent? node1) (not (get-field dependent? node2)))
;(further-wires (send node1 get-connected-nodes)))
; ((get-field dependent? node2) (further-wires (send node2 get-connected-nodes)))
;(else



; ));)))
