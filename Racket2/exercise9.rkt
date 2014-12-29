#lang racket
(define b1 true) 
(define b2 false) 

(define (truth a b)
  (and (equal? a #f) (equal? b #t)))