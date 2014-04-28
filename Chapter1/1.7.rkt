#lang racket

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

(define (sqrt-iter prev-guess curr-guess x)
  (if (good-enough? prev-guess curr-guess)
      curr-guess
      (sqrt-iter curr-guess
                 (improve curr-guess x)
                 x)))

(define (good-enough? prev-guess curr-guess)
  (< (abs (- prev-guess
             curr-guess))
     (* curr-guess 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))