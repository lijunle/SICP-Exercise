#lang racket

(define (fast-multiply a b)
  (define (iter n a b) ; ensure n + a * b = result
    (cond ((= b 0) n)
          ((even? b) (iter n
                           (double a)
                           (halve b)))
          (else (iter (+ n a)
                      a
                      (- b 1)))))
  (iter 0 a b))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))