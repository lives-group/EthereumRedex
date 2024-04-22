#lang racket


(require redex)

;; Υ → Ethereum state transition function.
;; σ → Valid state transition.
;; T → Transition. 
;; B → Block.
;; Π → Block-level state-transition function.


#|
(1) σ_(t+1) ≡  Υ(σ_t, T )
(2) σ_(t+1) ≡  Π(σ_t, B)
(3)       B ≡  (..., (T_0, T_1, ...), ...)
(4)       Π(σ, B) ≡  Υ(Υ(σ, T_0 ), T_1 )...
|#

(define-language Block
  [σ ::= Y
         Π]
  
  [T ::= x] ;; idk how to write it yet
  [Y ::= x] ;;           ||
  
  [B ::= ((T_0 T_1 ...) ...)]
  [Π ::= (Y_0 Y_1 ...)]
  [x ::= variable-not-otherwise-mentioned])


#|

Pag 30 -> começar por ai

mi = machine state

s calculation -> custo de cada instrucao na blockchain
(cada instrucao na blockchain tem um custo)

palavra no eth é um inteiro sem sinal com 256 bits

aritimetica no eth funciona igual corpo na algebra
2^256 - 1 + 1 = 0

and bit a bit com o 2^256
|#

(define um   #b0000000000000000000000000000000000000000000000000000000000000001)
(define tres #b0000000000000000000000000000000000000000000000000000000000000011)

;(ADD (1 0) (1)) --> (1 1)
;(ADD (1 0 0) (1 1)) --> (1 1 1)

#|

ETH ARITHMETIC IN PYTHON 

def add(computation: ComputationAPI) -> None:
    """
    Addition
    """
    left, right = computation.stack_pop_ints(2)

    result = (left + right) & constants.UINT_256_MAX

    computation.stack_push_int(result)


def addmod(computation: ComputationAPI) -> None:
    """
    Modulo Addition
    """
    left, right, mod = computation.stack_pop_ints(3)

    if mod == 0:
        result = 0
    else:
        result = (left + right) % mod

    computation.stack_push_int(result)

|#


(define-language ETH
  [E ::=
     STOP
     ADD 
     MUL 
     SUB 
     DIV
     MOD
     EXP]
  [b ::= number]
  [state ::= ((E ...) (E_1 ...) (b ...))]
  [stateElto ::= ((b ...) (E ...) b)]) ;; (pilha de numeros) (listra de instrucoes) instrucao corrente que esta executando 


(define red
  (reduction-relation
   ETH
   #:domain state

   ; ADD
   (--> ((E ...) (ADD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... ADD) (E_1 ...) (,(+ (term b_1) (term b_2)) b_3  ...)))

   ; SUB
   (--> ((E ...) (SUB E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... SUB) (E_1 ...) (,(abs (- (term b_1) (term b_2))) b_3 ...))) ; 

   ; MUL
   (--> ((E ...) (MUL E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... MUL) (E_1 ...) (,(* (term b_1) (term b_2)) b_3 ...)))
   
   
   ; DIV ZERO
   (--> ((E ...) (DIV E_1 ...) (b_1 0 b_3 ...))
        ((E ... DIV) (E_1 ...) (0  b_3 ...)))
   
   ; DIV
   (--> ((E ...) (DIV E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... DIV) (E_1 ...) (,(floor (/ (term b_1) (term b_2))) b_3 ...)))

    ; MOD ZERO
   (--> ((E ...) (MOD E_1 ...) (b_1 0 b_3 ...))
        ((E ... MOD) (E_1 ...) (0  b_3 ...)))
   
   ; MOD
   (--> ((E ...) (MOD E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... MOD) (E_1 ...) (,(round (/ (term b_1) (term b_2))) b_3 ...)))
   
  
   ; EXP
   (--> ((E ...) (EXP E_1 ...) (b_1 b_2 b_3 ...))
        ((E ... EXP) (E_1 ...) (,(expt (term b_1) (term b_2)) b_3 ...)))
   
   
   
   ))


(define-metafunction ETH
  funcDIV : b_1 -> b_2
  [(funcDIV b_1) ,(floor (term b_1))])

(define-metafunction ETH
  fetch : (E ...) number -> E
  [(fetch (E_1 E_2 ...) 0) E_1]
  [(fetch (E_1 E_2 ...) number) (fetch (E_2 ...) ,(- (term number) 1))])




(traces red (term ( () (DIV ADD EXP) (5 2 1 2))))
  
