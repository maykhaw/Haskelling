#lang racket
(define prefix "hello") 
(define suffix "world") 

(define (helloworld prefix suffix) 
  (string-append prefix "_" suffix)) 