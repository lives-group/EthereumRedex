#lang racket


(require redex)
(require "./constants.rkt")

(define-language ETH
  [E ::=
     STOP
     ADD 
     MUL 
     SUB 
     DIV
     SDIV
     MOD
     SMOD
     ADDMOD
     MULMOD
     EXP
     SIGNEXTEND
     LT
     GT
     SLT
     SGT
     EQ
     ISZERO
     AND
     OR
     XOR
     NOT
     BYTE
     SHL
     SHR
     SAR]
  [b ::= number]
  [bool ::= #f #t]
  [state ::= ((E ...) (E_1 ...) (b ...))])

(define red
  (reduction-relation
   ETH
   #:domain state

   (--> ((E ...) (ADD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... ADD) (E_1 ...) (,(bitwise-and  (+ (term b_1) (term b_2)) UNIT_256_MAX ) b_3  ...)) 
        "ADD")

   (--> ((E ...) (SUB E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... SUB) (E_1 ...) (,(bitwise-and (abs (- (term b_1) (term b_2))) UNIT_256_MAX) b_3 ...))
        "SUB") 

   (--> ((E ...) (MUL E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... MUL) (E_1 ...) (,(bitwise-and (* (term b_1) (term b_2)) UNIT_256_MAX) b_3 ...))
        "MUL")
   
   (--> ((E ...) (DIV E_1 ...) (b_1 0 b_3 ...))
        ((E ... DIV) (E_1 ...) (0  b_3 ...))
        "DIV-ZERO")
   
   (--> ((E ...) (DIV E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... DIV) (E_1 ...) (,(bitwise-and (floor (abs (/ (term b_1) (term b_2)))) UNIT_256_MAX) b_3 ...))
        "DIV")

   (--> ((E ...) (MOD E_1 ...) (b_1 0 b_3 ...))
        ((E ... MOD) (E_1 ...) (0  b_3 ...))
        "MOD-ZERO")
   
   (--> ((E ...) (MOD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... MOD) (E_1 ...) (,(modulo (term b_1) (term b_2)) b_3 ...))
        "MOD")
   
   (--> ((E ...) (ADDMOD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ...) (ADD MOD E_1 ...) (b_1 b_2 b_3 ...))
        "ADDMOD")
   
   (--> ((E ...) (MULMOD E_1 ...)  (b_1 b_2 b_3 ...))
        ((E ...) (MUL MOD E_1 ...) (b_1 b_2 b_3 ...))
        "MULMOD")
   
   (--> ((E ...) (LT E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... LT) (E_1 ...) ((funcBOOL ,(< (term b_1) (term b_2)))  b_3 ...))
        "LT")
   
   (--> ((E ...) (GT E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... GT) (E_1 ...) ((funcBOOL ,(> (term b_1) (term b_2)))  b_3 ...))
        "GT")
   
   (--> ((E ...) (EQ E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... EQ) (E_1 ...) ((funcBOOL ,(= (term b_1) (term b_2)))  b_3 ...))
        "EQ")   
   
   (--> ((E ...) (ISZERO E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... ISZERO) (E_1 ...) ((funcBOOL ,(zero? (term b_1) (term b_2)))  b_3 ...))
        "ISZERO")
   
   (--> ((E ...) (AND E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... AND) (E_1 ...) (,(bitwise-and (term b_1) (term b_2))  b_3 ...))
        "AND")
   
   (--> ((E ...) (OR E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... OR) (E_1 ...) ((funcBOOL ,(or (term b_1) (term b_2)))  b_3 ...))
        "OR")

   (--> ((E ...) (XOR E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... XOR) (E_1 ...) (,(bitwise-xor (term b_1) (term b_))  b_3 ...))
        "XOR")
   
   (--> ((E ...) (LT E_1 ...) (b_1 b_2 ...))
        ((E ... LT) (E_1 ...) ((funcNOT b_1) b_2 ...))
        "NOT")
   
   (--> ((E ...) (EXP E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... EXP) (E_1 ...) (,(expt (term b_1) (term b_2)) b_3 ...))
        "EXP")

   (--> ((E ...) (SDIV E_1 ...) (b_1 0 b_3 ...))
        ((E ... SDIV) (E_1 ...) (0 b_3 ...))
        "SDIV-ZERO")

   (--> ((E ...) (SDIV E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... SDIV) (E_1 ...) (,(floor (/ (term b_1) (term b_2))) b_3 ...))
        "SDIV")
   
   (--> ((E ...) (SMOD E_1 ...) (b_1 0 b_3 ...))
        ((E ... SMOD) (E_1 ...) (0 b_3 ...))
        "SMOD-ZERO")

   (--> ((E ...) (SMOD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... SMOD) (E_1 ...) (,(modulo (term b_1) (term b_2)) b_3 ...))
        "SMOD")

   (--> ((E ...) (SMOD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... SMOD) (E_1 ...) (,(modulo (term b_1) (term b_2)) b_3 ...))
        "BYTE")
   
   
   
   
   ))


(define-metafunction ETH
  funcNOT : b_1 -> b_2
  [(funcNOT 0)   1]
  [(funcNOT b_1) 0])


(define-metafunction ETH
  funcBOOL : bool -> b
  [(funcBOOL #f) 0]
  [(funcBOOL #t) 1])

(define-metafunction ETH
  fetch : (E ...) number -> E
  [(fetch (E_1 E_2 ...) 0) E_1]
  [(fetch (E_1 E_2 ...) number) (fetch (E_2 ...) ,(- (term number) 1))])




;(traces red (term (() (DIV ADD EXP) (5 2 1 2))))

