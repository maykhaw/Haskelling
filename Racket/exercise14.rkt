#lang racket 

; string-first is a function that extracts the first character in a string 

(define (string-first s)
	(string-ref s 0))

(string-first "the cows come home")

(define (cube-volume side)
	(expt side 3))

(define (cube-surface side)
	(* 6 (sqr side)))

(cube-volume 10)
(cube-surface 10)
