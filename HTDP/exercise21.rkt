#lang racket
(define (string-insert string ith) 
  (string-append 
   (substring string 0 ith)
   "-"
   (substring string ith )))