#lang racket
(define (all-true list) 
  (cond 
    [(empty? list) #t]
    [(cons? list)
     (and (first list)
          (all-true (rest list)))]))