#lang racket 

; string-first is a function that extracts the first character in a string 

(define (string-first s)
	(string-ref s 0))

(string-first "the cows come home")

