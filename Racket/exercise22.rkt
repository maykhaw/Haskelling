#lang racket 

(define (string-delete string1 i)
	(string-append 
		(substring string1 0 (- i 1))
		(substring string1 i (string-length string1))
	)
)

(string-delete "star_fishwife" 5)
