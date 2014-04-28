#lang racket

(define (pascal-triangle row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((> col row) 0)
        ((= col row) 1)
        (else (+ (pascal-triangle (- row 1) col)
                 (pascal-triangle (- row 1) (- col 1))))))