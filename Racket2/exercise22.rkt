#lang racket
(define (string-delete string ith)
  (string-append (substring string 0 (- ith 1)) (substring string ith (string-length string))))