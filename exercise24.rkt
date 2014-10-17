#lang racket 

(define fixedcost 180)
(define vcost 0.04)

(define (attendees tixprice)
	(- 120 (* (- tixprice 5.0) (/ 15 0.1))))

(define (revenue tixprice)
	(* tixprice (attendees tixprice)))

(define (cost tixprice)
	(+ fixedcost (* vcost (attendees tixprice))))

(define (profit tixprice)
	(- (revenue tixprice) (cost tixprice)))

(for ([i (in-range 1.0 5.0 0.1)])
	(print `(,i ,(profit i))))
