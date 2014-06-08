#lang racket

; provide fast-prime?

(define (fast-prime? n)
  (define (fast-prime-test n count)
    (cond ((= 0 count) true)
          ((fermat-test n) (fast-prime-test n (- count 1)))
          (else false)))
  (fast-prime-test n 3))

(define (fermat-test n)
  (define (try-it m)
    (= (expmod m n n) m))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp mod)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) mod))
                                mod))
        (else (remainder (* (expmod base (- exp 1) mod)
                            base)
                         mod))))

(define (square x)
  (* x x))

; procedure for this exercise

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n)
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

; exercises

(display "1e3")
(search-for-primes 1001 3)

(display "1e4")
(search-for-primes 10001 3)

(display "1e5")
(search-for-primes 100001 3)

(display "1e6")
(search-for-primes 1000001 3) ; 1e6

(display "1e7")
(search-for-primes 10000001 3) ; 1e7

(display "1e8")
(search-for-primes 100000001 3) ; 1e8

(display "1e9")
(search-for-primes 1000000001 3) ; 1e9

; Baiscally, meet the ratio 1:2
