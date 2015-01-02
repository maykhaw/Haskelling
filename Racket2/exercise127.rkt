#lang racket
(define (all-true list) 
  (cond 
    [(empty? list) #t]
    [(cons? list)
     (if (first list) (all-true (rest list)) #f)]))