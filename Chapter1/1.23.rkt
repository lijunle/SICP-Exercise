#lang racket

; inspired from exercise 1.21

(define (prime? n)
  (= (smallest-division n) n))

(define (smallest-division n)
  (find-division n 2))

(define (find-division n test-division)
  (cond ((divides? test-division n) test-division)
        ((> (square test-division) n) n)
        (else (find-division n (next test-division)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (next n)
  (if (= 2 n)
      3
      (+ 2 n)))

; inspired from exercise 1.22

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

; exercises on 1.22

(search-for-primes 1001 3)
(search-for-primes 10001 3)
(search-for-primes 100001 3)
(search-for-primes 1000001 3)

(search-for-primes 1000000001 3) ; 1e9
(search-for-primes 10000000001 3) ; 1e10
(search-for-primes 100000000001 3) ; 1e11
(search-for-primes 1000000000001 3) ; 1e12

; the ratio with exercise 1.22 is between 1.3 to 1.8
; the IF statement if NEXT procedure needs extra cost
