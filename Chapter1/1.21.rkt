#lang racket

(provide prime?)

(define (smallest-division n)
  (find-division n 2))

(define (find-division n test-division)
  (cond ((divides? test-division n) test-division)
        ((> (square test-division) n) n)
        (else (find-division n (+ 1 test-division)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (= (smallest-division n) n))

; exercise

(smallest-division 199)

(smallest-division 1999)

(smallest-division 19999)
