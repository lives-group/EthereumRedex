#lang racket

(require redex)
(require "./main.rkt")



;(traces red (term (() (ADD DIV SUB) () (5 2 1 2))))


(traces red (term (() (ADDRESS BALANCE ORIGIN ADD) (1 2 3 4 5 6 7 8 9 10) (11))))
