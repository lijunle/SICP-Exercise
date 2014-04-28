#lang racket

(define (sum-of-max-two a b c)
  (- (+ a b c)
     (cond ((and (<= a b) (<= a c)) a)
           ((and (<= b a) (<= b c)) b)
           (else c))))