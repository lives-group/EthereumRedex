#lang racket


(require redex)
(require "./constants.rkt")
(provide (all-defined-out))


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
     SAR
     ;-----
     ADDRESS
     BALANCE
     ORIGIN
     CALLER
     CALLVALUE
     CALLVALUE
     CALLDATALOAD
     CALLDATASIZE
     CALLDATACOPY
     CODESIZE
     CODECOPY
     GASPRICE
     EXTCODESIZE
     EXTCODECOPY
     RETURNDATASIZE
     RETURNDATACOPY
     EXTCODEHASH]
  [b ::= number]
  [addressType ::= (side-condition (name n number) (< (integer-length (term n)) 160))]
  [bool ::= #f #t]
  [Ia Io Is::= addressType] 
 ; [Io ::= addressType] ; Sender - acredito que tambem seja um endereço 
  [Ip ::= number] ; Price of gas paid - Pag 8
  [Id ::= number] ; the byte array that is the input data to this execution if the execution agent is a transaction, this would be the transaction data.
  ;[Is ::= addressType] ;; Outro endereço
  [Iv ::= integer] ; value in Wei
  [Ib ::= number] ; Pensei em puxar o valor da lista dos ja executados
  [Ih ::= number ] ; The block header of the present block.
  [Ie ::= number] ; Olhar na lista dos ja executados e ver quantos calls or create(2) tem
  [Iw ::= number ] ; permission
  [I  ::= (I ...)
      Ia Io Ip Id Is Iv Ib Ih Ie Iw]
  
  [state ::= ((E ...) (E_1 ...) I (b ...))])


(define red
  (reduction-relation
   ETH
   #:domain state

   (--> ((E ...) (STOP E_1 ...) I (b ...))
        ;((E ... STOP) (E_1 ...) I (b ...)) 
        "STOP")
   
   (--> ((E ...) (ADD E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... ADD) (E_1 ...) I (,(bitwise-and  (+ (term b_1) (term b_2)) UNIT_256_MAX ) b_3  ...)) 
        "ADD")

   (--> ((E ...) (SUB E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SUB) (E_1 ...) I (,(bitwise-and (abs (- (term b_1) (term b_2))) UNIT_256_MAX) b_3 ...))
        "SUB") 

   (--> ((E ...) (MUL E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... MUL) (E_1 ...) I (,(bitwise-and (* (term b_1) (term b_2)) UNIT_256_MAX) b_3 ...))
        "MUL")
   
   (--> ((E ...) (DIV E_1 ...) I (b_1 0 b_3 ...))
        ((E ... DIV) (E_1 ...) I (0  b_3 ...))
        "DIV-ZERO")
   
   (--> ((E ...) (DIV E_1 ...) I  (b_1 b_2 b_3 ...))
        ((E ... DIV) (E_1 ...) I (,(bitwise-and (floor (abs (/ (term b_1) (term b_2)))) UNIT_256_MAX) b_3 ...))
        "DIV")

   (--> ((E ...) (MOD E_1 ...) I (b_1 0 b_3 ...))
        ((E ... MOD) (E_1 ...) I (0  b_3 ...))
        "MOD-ZERO")
   
   (--> ((E ...) (MOD E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... MOD) (E_1 ...) I (,(modulo (term b_1) (term b_2)) b_3 ...))
        "MOD")
   
   (--> ((E ...) (ADDMOD E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ...) (ADD MOD E_1 ...) I (b_1 b_2 b_3 ...))
        "ADDMOD")
   
   (--> ((E ...) (MULMOD E_1 ...) I  (b_1 b_2 b_3 ...))
        ((E ...) (MUL MOD E_1 ...) I (b_1 b_2 b_3 ...))
        "MULMOD")
   
   (--> ((E ...) (LT E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... LT) (E_1 ...) I ((funcBOOL ,(< (term b_1) (term b_2)))  b_3 ...))
        "LT")

   (--> ((E ...) (SLT E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SLT) (E_1 ...) I ((funcBOOL ,(< (term b_1) (term b_2)))  b_3 ...))
        "SLT")

   (--> ((E ...) (SGT E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SGT) (E_1 ...) I ((funcBOOL ,(> (term b_1) (term b_2)))  b_3 ...))
        "SGT")
   
   (--> ((E ...) (GT E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... GT) (E_1 ...) I ((funcBOOL ,(> (term b_1) (term b_2)))  b_3 ...))
        "GT")
   
   (--> ((E ...) (EQ E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... EQ) (E_1 ...) I ((funcBOOL ,(= (term b_1) (term b_2)))  b_3 ...))
        "EQ")   
   
   (--> ((E ...) (ISZERO E_1 ...) I (b_1 b_2  ...))
        ((E ... ISZERO) (E_1 ...) I ((funcBOOL ,(zero? (term b_1))) b_2 ...))
        "ISZERO")
   
   (--> ((E ...) (AND E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... AND) (E_1 ...) I (,(bitwise-and (term b_1) (term b_2))  b_3 ...))
        "AND")
   
   (--> ((E ...) (OR E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... OR) (E_1 ...) I (,(bitwise-ior (term b_1) (term b_2))  b_3 ...))
        "OR")

   (--> ((E ...) (XOR E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... XOR) (E_1 ...) I (,(bitwise-xor (term b_1) (term b_2))  b_3 ...))
        "XOR")
   
   (--> ((E ...) (NOT E_1 ...) I (b_1 b_2 ...))
        ((E ... NOT) (E_1 ...) I ((funcNOT b_1) b_2 ...))
        "NOT")
   
   (--> ((E ...) (EXP E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... EXP) (E_1 ...) I (,(expt (term b_1) (term b_2)) b_3 ...))
        "EXP")

   (--> ((E ...) (SDIV E_1 ...) I (b_1 0 b_3 ...))
        ((E ... SDIV) (E_1 ...) I (0 b_3 ...))
        "SDIV-ZERO")

   (--> ((E ...) (SDIV E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SDIV) (E_1 ...) I (,(floor (/ (term b_1) (term b_2))) b_3 ...))
        "SDIV")
   
   (--> ((E ...) (SMOD E_1 ...) I (b_1 0 b_3 ...))
        ((E ... SMOD) (E_1 ...) I (0 b_3 ...))
        "SMOD-ZERO")

   (--> ((E ...) (SMOD E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SMOD) (E_1 ...) I (,(modulo (term b_1) (term b_2)) b_3 ...))
        "SMOD")

   (--> ((E ...) (BYTE E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... BYTE) (E_1 ...) I (0 b_3 ...))
        (side-condition (>= (term b_1) 32))
        "BYTE-BT-32")

   (--> ((E ...) (BYTE E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... BYTE) (E_1 ...) I (,(modulo (quotient (term b_2) (expt 256 (- 31 (term b_1))))) b_3 ...))
        (side-condition (< (term b_1) 32)) 
        "BYTE-ST-32")
   
   (--> ((E ...) (SHL E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SHL) (E_1 ...) I (,(bitwise-and  (arithmetic-shift (term b_2) (* -1 (term b_2))) UNIT_256_MAX ) b_3  ...)) 
        "SHL")

   (--> ((E ...) (SHR E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SHR) (E_1 ...) I (,(abs (arithmetic-shift (term b_1) (term b_2)))  b_3  ...)) 
        "SHR")   

   (--> ((E ...) (SAR E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SAR) (E_1 ...) I (,(arithmetic-shift (term b_1) (term b_2))  b_3  ...)) 
        "SAR")
   
   (--> ((E ...) (SIGNEXTEND E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SIGNEXTEND) (E_1 ...) I (b_2  b_3  ...)) 
        "SIGNEXTEND-BT-31"
        (side-condition (< 31 (term b_1))))
   
   (--> ((E ...) (SIGNEXTEND E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SIGNEXTEND) (E_1 ...) I (,(bitwise-ior (term b_2) (- UNIT_256_CEILING (arithmetic-shift -1 (+ (* (term b_1) 8) 7)))) b_3  ...)) 
        "SIGNEXTEND-SL-31"
        (side-condition (and (<= (term b_1) 31)
                             (bitwise-and (term b_2) (arithmetic-shift -1 (+ (* (term b_1) 8) 7))))))

   (--> ((E ...) (SIGNEXTEND E_1 ...) I (b_1 b_2 b_3 ...))
        ((E ... SIGNEXTEND) (E_1 ...) I (,(bitwise-and (term b_2) (- (arithmetic-shift -1 (+ (* (term b_1) 8) 7)) 1)) b_3  ...)) 
        "SIGNEXTEND-SL-31-else"
        (side-condition (and (<= (term b_1) 31)
                             (not (bitwise-and (term b_2) (arithmetic-shift -1 (+ (* (term b_1) 8) 7)))))))


   ; ===========================
   
   (--> ((E ...) (ADDRESS E_1 ...) (Ia I_2 ...) (b_1 ...))
        ((E ... ADDRESS) (E_1 ...) (Ia I_2 ...) (Ia b_1 ...))
        "ADDRESS")

   (--> ((E ...) (BALANCE E_1 ...) I (b_1 b_2 ...))
        ((E ... BALANCE) (E_1 ...) I (,(modulo (term b_1) (expt 2 160)) b_2 ...))
        "BALANCE"
        (side-condition (not (zero? (modulo (term b_1)
                                             (expt 2 160))))))
   
   (--> ((E ...) (BALANCE E_1 ...) I (b_1 b_2 ...))
        ((E ... BALANCE) (E_1 ...) I (0 b_2 ...))
        "BALANCE-OTHERWISE"
         (side-condition (zero? (modulo (term b_1)
                                             (expt 2 160)))))
   
   (--> ((E ...) (ORIGIN E_1 ...)  (Ia Io I ...) (b_1 ...))
        ((E ...   ORIGIN) (E_1 ...) (Ia Io I ...) (Io b_1 ...))
        "ORIGIN")

   (--> ((E ...) (CALLER E_1 ...) (Ia Io Ip Id Is Iv Ib Ih Ie Iw) (b_1 b_2 ...))
        ((E ... CALLER) (E_1 ...) I (Is b_1 b_2 ...))
        "CALLER")

   (--> ((E ...) (CALLVALUE E_1 ...) (Ia Io Ip Id Is Iv Ib Ih Ie Iw) (b_1 b_2 ...))
        ((E ... CALLVALUE) (E_1 ...) I (Iv b_1 b_2 ...))
        "CALLVALUE")

   (--> ((E ...) (CALLDATALOAD E_1 ...) (Ia Io Ip Id Is Iv Ib Ih Ie Iw) (b_1 b_2 ...))
        ((E ... CALLDATALOAD) (E_1 ...) (Ia Io Ip (calldataload_ Id (term b_1) 0) Is Iv Ib Ih Ie Iw) (b_1 b_2 ...))
        "CALLDATALOAD")

   (--> ((E ...) (CALLDATASIZE E_1 ...) (Ia Io Ip Id Is Iv Ib Ih Ie Iw) (b_1 b_2 ...))
        ((E ... CALLDATASIZE) (E_1 ...) I (,(length (term Id)) b_1 b_2 ...))
        "CALLDATASIZE")   


   
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

(define-metafunction ETH
  calldataload_ : Id b_1 b_2 -> (b ...)
  [(calldataload_ (b ...) b_1 32) (b ...)]
  [(calldataload_ (b ...) b_1 b_2) (calldataload_ (b ... ,(+ (term b_1) (term b_2))) b_1 ,(+ (term b_2) 1))])




