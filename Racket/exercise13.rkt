#lang racket

(define (distance x y) 
	(sqrt (+ (sqr x) (sqr y))))

(distance 3 4) 
