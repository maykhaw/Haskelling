#lang racket
(define (contains? object list)
  (cond
    [(empty? list) false]
    [(cons? list)
     (or (equal? (first list) object)
         (contains? object (rest list)))]))