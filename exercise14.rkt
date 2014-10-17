#lang racket 

(define (cube-volume side)
	(expt side 3))

(define (cube-surface side)
	(* 6 (sqr side)))

(cube-volume 10)
(cube-surface 10)
