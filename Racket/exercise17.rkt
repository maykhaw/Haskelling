#lang racket 

(define (bool-imply b1 b2)
	(if b1 b2 #t))

(equal? (bool-imply #t #t) #t)
(equal? (bool-imply #t #f) #f)
(equal? (bool-imply #f #t) #t)
(equal? (bool-imply #f #f) #t)
