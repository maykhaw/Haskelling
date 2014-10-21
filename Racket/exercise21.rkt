#lang racket 

(define (string-insert string1 i)
	(string-append 
		(substring string1 0 i) 
		"_" 
		(substring string1 i (string-length string1))
	)
)

(string-insert "starfishwife" 0)
 
(string-insert "starfishwife" 22)
(string-insert "starfishwife" 4)
