#lang racket

(require redex)
(provide (all-defined-out))


;; Constants

(define UNIT_256_MAX (- (expt 2 256) 1))
