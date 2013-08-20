#lang racket

(define (transpose m)
  (if (check m) '(() () ())
      (zip (car m) (transpose (cdr m)))))

(define (zip l1 l2)   
  (if (null? l1) '()
      (append (list (append (list (car l1)) (car l2))) (zip (cdr l1) (cdr l2)))))

(define (check l)
  (define (helper l1 i)
     (cond
       ((= i (length l)) #t)
       ((not (null? (car l1))) #f)
       (else (helper (cdr l1) (+ i 1)))))
  (helper l 0))

(define (make-B-matrix)
  (define lst-of-nodes (create-UVN-list))
  (define B (make-2d-vec (length lst-of-nodes) no-of-batteries #f))
  
  (define (B-assist i j)
    (cond (< i (length lst-of-nodes))
          (cond ((equal? (vector-ref node-vec i) (send (vector-ref battery-vec j) (get-node1))) 
                 (begin (2d-vector-set! B i j 1) (B-assist (+ i 1) (+ j 1))))                
                ((equal? (vector-ref node-vec i) (send (vector-ref battery-vec j) (get-node2))) 
                 (begin (2d-vector-set! B i j -1) (B-assist (+ i 1) (+ j 1))))
                (else (begin (2d-vector-set! i j 0) (B-assist (+ i 1) (+ j 1))))))
    
    (B-assist 0 0)))

(define (make-C-matrix)
  (define C (list->vector (transpose (vector->list B)))))

(define (make-D-matrix)
  (define D (make-2d-vector no-of-batteries no-of-batteries 0)))

(define (make-A-matrix)
(define A (list->vector (mmap (λ(x) (list->vector x)) 
                              (zip (append (vector->list (mmap (λ(x) (vector->list x)) G)) 
                                           (vector->list (mmap (λ(x) (vector->list x)) C))) 
                                   (append (vector->list (mmap (λ(x) (vector->list x)) B)) 
                                           (vector->list (mmap (λ(x) (vector->list x)) D))))))))