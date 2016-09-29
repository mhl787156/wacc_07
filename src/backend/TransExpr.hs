module TransExpr where

import Asm
import Grammar
import AllocateRegisters
import Util

import qualified Data.Map as Map
import Data.Maybe
import Data.Char

--translate an expression, leaves the result in reg0
translateExpr :: Expr -> ProgState -> ProgState
translateExpr (IntLiteral n) pst --int literal
  = pst ++= [LDR W AL reg0 (IntAdd n)]

translateExpr (BoolLiteral b) pst --bool literal
  = pst ++= [MOV No AL reg0 (ImmInt n)]
  where
    n = if b then 1 else 0

translateExpr (CharLiteral c) pst --char literal
  = pst ++= [LDR W AL reg0 (IntAdd (ord c))]

translateExpr (StringLiteral str) pst@(_,_,Registers (r:_)) --string literal
  = pst 
    ==> (++= [StoreString r str])

translateExpr (PairLiteral p) pst@(_,_,Registers (r:rs)) --pair literal
  = pst
    ==> (++= [MOV No AL reg0 (ImmInt 0)])

--translate an array element, with lookup
translateExpr (ArrayElemExpr (ArrayElem ident [])) pst@(is,st,rs) --base
  = (is', Map.delete (Id "&&&", 0) st', rs)
  where
    (reg, t) = lookupInSymbolTable ident pst --the final result is stored at 
                                             --"&&&" after recursion
    (is',st',rs') = pst ++= [MOV No AL reg0 (ShiftedRegister reg NOS)]
translateExpr (ArrayElemExpr (ArrayElem ident (e:es))) 
                                            pst@(_,_,Registers (r:rs))--recurse
  = pst
    ==> translateExpr e
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)])
    -- pass array to OutOfBounds in r1
    ==> (++= [MOV No AL (FinalGpReg 1) (ShiftedRegister reg NOS)]) 
    ==> (++= [OutOfBoundsRuntimeError])
    ==> (++= [MOV No AL reg0 (ShiftedRegister r NOS)])
    ==> (++= [ADD No AL reg0 reg0 (ImmInt inc)]) -- increment to avoid size
    ==> accessArrayO reg reg0 t       -- loads [reg + (reg0*4)] into reg0
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)])
    ==> insertSymbolTableValue (Id "&&&") (ArrayType (ArrayInstance AnyType))
    ==> translateExpr (ArrayElemExpr (ArrayElem (Id "&&&") es))
    ==> (\(is',st',rs')->(is',st',Registers (r:rs)))
    where
      (reg, t) = lookupInSymbolTable ident pst -- array is stored in reg
      inc = case t of BaseType _ -> 4 --strings need to add 4 bytes to miss size
                      _          -> 1 --arrays need to add 1 word to miss size

--translate id statement, looks up in symbol table
translateExpr (IdentExpr ident) pst@(is,st,rs)
  = pst ++= [MOV No AL reg0 (ShiftedRegister reg NOS)]
  where
    (reg,_) = lookupInSymbolTable ident pst

--translate logical not operator
translateExpr (UnOpExpr FactOp e) pst -- logical not operator
  = pst
    ==> translateExpr e
    ==> (++= [EOR No AL reg0 reg0 (ImmInt 1)])

--translate negation of int
translateExpr (UnOpExpr NegOp e) pst@(_,_,Registers (r:rs))
  = pst
    ==> translateExpr e                        --translate e into reg0
    ==> (++= [MOV No AL r (ImmInt 0),
              SUB Yes AL reg0 r (ShiftedRegister reg0 NOS), --sub 0 from ro
              IntegerOverflowUnderflowRuntimeError]) 
--translate length operator
translateExpr (UnOpExpr LenOp e) pst 
  = pst 
    ==> translateExpr e
    ==> accessArrayN reg0 (-1) -- -1 as accessarray adds one for ease of use)
--translate ord operator, changes nothing, just treats results as int
translateExpr (UnOpExpr OrdOp e) pst
  = pst ==> translateExpr e
--translate chr operator, changes nothing, just treats results as char
translateExpr (UnOpExpr ChrOp e) pst
  = pst ==> translateExpr e

--translate division operator
translateExpr (BinOpExpr DivOp  e1 e2) pst@(_,_,Registers (r:r':rs))
  = pst
   ==> binOpMain e2 e1
   ==> divide r' r
   ==> (++= [IntegerOverflowUnderflowRuntimeError])

--translate the minus operator
translateExpr (BinOpExpr MinOp  e1 e2) pst@(_,_,Registers (r:r':rs))
  = pst
   ==> binOpMain e2 e1
   ==> (++= [SUB Yes AL reg0 r' (ShiftedRegister r NOS), 
             IntegerOverflowUnderflowRuntimeError]) --subtract r from reg0

translateExpr (BinOpExpr PlusOp e1 e2) pst@(_,_,Registers (r:r':rs))
  = pst
   ==> binOpMain e2 e1
   ==> (++= [ADD Yes AL reg0 r' (ShiftedRegister r NOS), 
             IntegerOverflowUnderflowRuntimeError]) --add r to reg0
                                      
translateExpr (BinOpExpr operator e1 e2) pst@(_,_,Registers (r:r':rs))
    = pst
     ==> binOpMain e1 e2
     ==> binOpSpecific
      where
        binOpSpecific = case operator of MultOp -> (++= [MUL AL (FinalGpReg 0) (FinalGpReg 1) r r',
                                                     MOV No AL r' (ShiftedRegister (FinalGpReg 0) NOS),
                                                     CMP AL (FinalGpReg 1) (ShiftedRegister (FinalGpReg 0) (ASR 31)),
                                                     MulOverflowUnderflowRuntimeError, --check for overflow
                                                     MOV No AL reg0 (ShiftedRegister r' NOS)]) 
                                         ModOp  -> (++= [MOV No AL (FinalGpReg 0) (ShiftedRegister r NOS), 
                                                         MOV No AL (FinalGpReg 1) (ShiftedRegister r' NOS), 
                                                         CMP AL (FinalGpReg 1) (ImmInt 0), 
                                                         DivideByZeroRuntimError, 
                                                         BL AL "__aeabi_idivmod", 
                                                         MOV No AL reg0 (ShiftedRegister (FinalGpReg 1) NOS), 
                                                         IntegerOverflowUnderflowRuntimeError])
--                                         PlusOp -> (++= [ADD Yes AL reg0 r' (ShiftedRegister r NOS), 
--                                                         IntegerOverflowUnderflowRuntimeError]) --add r to reg0
                                         GTOp   -> (++= [CMP AL r (ShiftedRegister r' NOS), --compare e1, e2
                                                         MOV No AL reg0 (ImmInt 0),     --move 0 (false) to reg 0
                                                         MOV No GThan reg0 (ImmInt 1)])  --move 1 to reg0 if GT
                                         GTEqOp -> (++= [CMP AL r (ShiftedRegister r' NOS), --compare e1, e2
                                                          MOV No AL reg0 (ImmInt 0),     --move 0 (false) to reg 0
                                                          MOV No GE reg0 (ImmInt 1)])     --move 1 to reg0 if GE
                                         LTOp   -> (++= [CMP AL r (ShiftedRegister r' NOS), --compare e1, e2
                                                          MOV No AL reg0 (ImmInt 0),     --move 0 (false) to reg 0
                                                          MOV No LThan reg0 (ImmInt 1)])  --move 1 to reg0 if LT
                                         LTEqOp -> (++= [CMP AL r (ShiftedRegister r' NOS), --compare e1, e2
                                                         MOV No AL reg0 (ImmInt 0),     --move 0 (false) to reg 0
                                                         MOV No LE reg0 (ImmInt 1)])     --move 1 to reg0 if LE
                                         EqOp   -> (++= [CMP AL r (ShiftedRegister r' NOS), --compare e1, e2
                                                         MOV No AL reg0 (ImmInt 0),     --move 0 (false) to reg 0
                                                         MOV No EQu reg0 (ImmInt 1)])    --move 1 to reg0 if EQu
                                         NEqOp  -> (++= [CMP AL r (ShiftedRegister r' NOS), --compare e1, e2
                                                         MOV No AL reg0 (ImmInt 0),     --move 0 (false) to reg 0
                                                         MOV No NEq reg0 (ImmInt 1)])    --move 1 to reg0 if NEq
                                         AndOp  -> (++= [AND No AL reg0 r' (ShiftedRegister r NOS)]) --AND reg0 with r
                                         OrOp   -> (++= [ORR No AL reg0 r' (ShiftedRegister r NOS)]) --ORR reg0 with r
                                         _      -> error ""
--bracketed expression is evaluated normally
translateExpr (BracketedExpr e) pst
  = translateExpr e pst

-- Generic part of assembly code for translating binop expressions
binOpMain :: Expr -> Expr -> ProgState -> ProgState
binOpMain e1 e2 pst@(_,_,Registers (r:r':rs))
 = pst
   ==> translateExpr e1                      --translate first exp into reg0
   ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)]) --move reg0 to memory
   ==> removeTopRegister
   ==> translateExpr e2                      --translate second exp into reg0
   ==> putRegisterOnTop r
   ==> (++= [MOV No AL r' (ShiftedRegister reg0 NOS)]) --move reg0 to memory
