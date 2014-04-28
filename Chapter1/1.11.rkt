#lang racket

; recursive style

(define (f-recursive n)
  (define (f n)
    (if (<= n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3))))))
  (f n))

; iterative style

(define (f-iterative n)
  (define (f a b c count)
    (if (= count 0)
        c
        (f b
           c
           (+ c
              (* 2 b)
              (* 3 a))
           (- count 1))))
  (if (<= n 3)
      n
      (f 1 2 3 (- n 3))))