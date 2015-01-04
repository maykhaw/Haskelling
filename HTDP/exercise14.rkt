#lang racket
(define (cube-volume x) 
  (expt x 3))

(define (cube-surface x) 
  (* 6 (* x x)))