#lang racket

(require "1.21.rkt")

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes odd-number count)
  (cond ((= 0 count) (newline))
        ((timed-prime-test odd-number) (search-for-primes (+ 2 odd-number) (- count 1)))
        (else (search-for-primes (+ 2 odd-number) count))))

; exercises, these numbers are too small to get 1ms

(search-for-primes 1001 3)
(search-for-primes 10001 3)
(search-for-primes 100001 3)
(search-for-primes 1000001 3)

; make the numbers bigger

(search-for-primes 1000000001 3) ; 1e9
(search-for-primes 10000000001 3) ; 1e10
(search-for-primes 100000000001 3) ; 1e11
(search-for-primes 1000000000001 3) ; 1e12

; Basically, the increase satisfies sqrt(10) speed
