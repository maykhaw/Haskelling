#lang racket
(define (pos? list)
  (cond
    [(empty? list) #t]
    [(cons? list)
     (and (>= (first list) 0)
         (pos? (rest list)))]))