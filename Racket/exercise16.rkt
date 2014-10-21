#lang racket 

; string-first is a function that extracts the first character in a string 

(define (string-last s)
	(string-ref s (- (string-length s) 1)))

(string-last "the cows come home")

