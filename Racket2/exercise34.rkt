#lang racket
(define (string-remove-last string)
  (substring string 0 (- (string-length string) 1)))