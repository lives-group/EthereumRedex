#lang racket

(require redex)
(require "./main.rkt")


#;(traces red (term (() (DIV ADD EXP) (5 2 1 2))))

(traces red (term (() (ADD MUL SUB DIV SDIV MOD SMOD ADDMOD MULMOD EXP SIGNEXTEND LT STOP SAR) (52 78 12 45 96 33 11 27 84 21 68 7 91 3 ))))

#;(traces red (term (() (GT SLT EQ ISZERO AND OR XOR NOT SHL SHR) (64 99 41 15 39 73 2 66 5 18 82 484 54 699 454 123 455))))

;; TODO: OLHAR DIV 0 BYTE

