#lang racket
(define (juxtapose list) 
  (if (empty? list) null (string-append (first list) (juxtapose (rest list)))))