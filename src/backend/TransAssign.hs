module TransAssign where

import Asm
import Grammar
import AllocateRegisters
import Util
import TransExpr

import qualified Data.Map as Map
import Data.Maybe
import Data.List


-- translate an AssignRHS
translateAssignRhs :: AssignRhs -> ProgState -> ProgState

-- Translates a Right Hand side of just expression
-- e.g. an identity ? = 3 + 3 
translateAssignRhs (AssignExpr e) pst
  = translateExpr e pst

-- Translates a Right Hand SIde of type array literal
-- e.g. an identity ? = [0,1,2]
-- Recursively translates the array elements
-- creates the array and stores it on the heap and returns the 
-- reference to the array in register 0
translateAssignRhs (AssignArrayLit (ArrayLit es)) pst@(is, st, registers)
  = (is', st', registers)
  where 
    (rs ,i ,progstate) = helper es [] 0 pst
    (is', st',_) = progstate ==> loadArray rs i

    helper :: [Expr] -> [Register] -> Int -> ProgState -> ([Register], Int, ProgState)
    helper [] rs i (is, st, Registers (r:regs)) 
      = ( r:reverse rs , i+1 , (is ++ [MOV No AL r (ImmInt i)] ,st , Registers regs) )
    helper (e:es) rs i pst@(is, st, Registers (r:_)) 
      = helper es (r:rs) (i+1) (pst' ++= [MOV No AL r (ShiftedRegister reg0 NOS)])
      where         
      pst' = translateExpr e (removeTopRegister pst)

-- Translates a Right Hand SIde of type new pair
-- e.g. and ? = newpair(0,1)
-- Recursively translates pair elemnts
-- creates the array and allocates memory for the inner blocks and the pair itself,
-- reurns reference to pair in register 0
translateAssignRhs (AssignNewPair e1 e2) pst@(_, _, Registers (r1:r2:rs))
  = pst 
    ==> removeTopRegister . removeTopRegister
    ==> translateExpr e1
    ==> (++= [MOV No AL r1 (ShiftedRegister reg0 NOS)])
    ==> allocateBlock 1
    ==> (++= [STR W AL r1 (OffsetReg reg0 (IntOffset 0)),
              MOV No AL r1 (ShiftedRegister reg0 NOS)])
    ==> translateExpr e2
    ==> (++= [MOV No AL r2 (ShiftedRegister reg0 NOS)])
    ==> allocateBlock 1
    ==> (++= [STR W AL r2 (OffsetReg reg0 (IntOffset 0)),
              MOV No AL r2 (ShiftedRegister reg0 NOS)])
    ==> loadPairs (r1,r2)
    ==> putRegisterOnTop r1 . putRegisterOnTop r2 

-- Translates an assignment to fst element of a pair
-- e.g. ? = fst(x) where x is a pair
translateAssignRhs (AssignPairElem (PairFst e)) pst
  = pst
    ==> translateExpr e
    ==> (++= [NullPointerRuntimeError])
    ==> loadFromPair reg0 1

-- Translates an assignment to snd element of a pair
-- e.g. ? = snd(x) where x is a pair
translateAssignRhs (AssignPairElem (PairSnd e)) pst
  = pst 
    ==> translateExpr e
    ==> (++= [NullPointerRuntimeError])
    ==> loadFromPair reg0 2

-- Translates an assignment to a function call
-- Recuversively adds arguments to the stack
-- transaltes all
-- Calls the given function and returns 
-- value in register 0
translateAssignRhs (AssignCall (Id id) es) pst 
  = pst
    ==> pushParameters es' 0
    ==> (++= [BL AL ("func_"++id),
              ADD No AL StackPointer StackPointer (ImmInt (length es*4))])
  where
    es' = reverse es --push parameters in reverse order

    --push a reversed list of parameters to the stack
    pushParameters :: [Expr] -> Int -> ProgState -> ProgState
    pushParameters [] count pst = pst ++= [SUB No AL StackPointer StackPointer (ImmInt (4*count))]
    pushParameters (e:es) count pst
      = pst
        ==> translateExpr e
        ==> (++= [SUB No AL StackPointer StackPointer (ImmInt (4*count))])
        ==> (++= [PUSH AL (Registers [FinalGpReg 0])])
        ==> (++= [ADD No AL StackPointer StackPointer (ImmInt (4*count + 4))])
        ==> pushParameters es (count+1)

-- translate an AssignLHS
translateAssignLhs :: AssignLhs -> ProgState -> ProgState

-- Translates an assignment to an identity
-- e.g. x = ?
translateAssignLhs (AssToIdent ident) pst@(_,st,_)
  = case ident of 
    SElemId strId attrId 
        -> pst ++= [STR W AL reg0 (OffsetReg r (IntOffset ((index+1)*4)))]
        where
          stAttrs  = head [list | StructBlock x list <- stList, x == strId]
          index    = fromJust $ elemIndex attrId (map fst stAttrs)
    _   -> pst ++= [MOV No AL r'' (ShiftedRegister reg0 NOS)]
    where 
      (r, _) = lookupInSymbolTable ident pst
      stList   = map fst $ map fst $ Map.toList st
      stBlocks = [e | e@(StructBlock x _ ) <- stList, x == ident]
      (r', _)= lookupInSymbolTable (head stBlocks) pst
      r''    = if null stBlocks then r else r'

-- Translates assignment ot a n element of an array
-- e.g. x[0] = ? where x is an multidimensional array
translateAssignLhs (AssToArray (ArrayElem ident es)) pst@(_,_,Registers (r:rs))
  = pst
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)]) -- move assignRHS into register r
    ==> insertSymbolTableValue (Id "&&&assign") AnyType    -- keep track of assignRHS
    ==> translateAssignHelper (AssToArray (ArrayElem ident es))

  where
    translateAssignHelper (AssToArray (ArrayElem ident [])) pst
      = pst
    translateAssignHelper (AssToArray (ArrayElem ident [e])) pst
      = pst
        ==> translateExpr e
        ==> (++= [MOV No AL (FinalGpReg 1) (ShiftedRegister reg NOS)]) -- pass array to OutOfBounds in r1
        ==> (++= [OutOfBoundsRuntimeError])
        ==> (++= [ADD No AL reg0 reg0 (ImmInt inc)]) -- increment to avoid size
        ==> storeIntoArrayO r reg reg0 t
      where
        (reg, t) = lookupInSymbolTable ident pst -- array is stored in reg
        (r,_) = lookupInSymbolTable (Id "&&&assign") pst --lookup value to store
        inc = case t of BaseType _ -> 4 --strings need to add 4 bytes to miss size
                        _          -> 1 --arrays need to add 1 word to miss size
    translateAssignHelper (AssToArray (ArrayElem ident (e:es))) pst@(_,_,Registers (r:rs))
      = pst
        ==> translateExpr e                 -- Stores value of index at reg0
        ==> (++= [MOV No AL (FinalGpReg 1) (ShiftedRegister reg NOS)]) -- pass array to OutOfBounds in r1
        ==> (++= [OutOfBoundsRuntimeError])
        ==> (++= [ADD No AL reg0 reg0 (ImmInt inc)]) -- increment to avoid size
        ==> accessArrayO reg reg0 t       -- loads [reg + (reg0*4)] into reg0
        ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)])                   -- keep track of loaded value,
        ==> insertSymbolTableValue (Id "&&&") (ArrayType (ArrayInstance AnyType)) -- might be multidimensional
        ==> translateAssignHelper (AssToArray (ArrayElem (Id "&&&") es)) --recurse through other dimensions
      where
        (reg, t) = lookupInSymbolTable ident pst -- array is stored in reg
        inc = case t of BaseType _ -> 4 --strings need to add 4 bytes to miss size
                        _          -> 1 --arrays need to add 1 word to miss size


-- Translates an assignment to the fst element of a pair
-- e.g. fst(x) = ? where x is a pair
translateAssignLhs (AssToPair (PairFst e)) pst@(_,_,Registers (r:r':rs))
  = pst
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)]) -- move assignRHS into register r
    ==> translateExpr e                 --reference to pair stored in reg0
    ==> (++= [NullPointerRuntimeError])
    ==> (++= [LDR W AL r' (OffsetReg reg0 (IntOffset 4))])
    ==> storeIntoPair r r' 0

-- Translates an assignment to the snd element of a pair
-- e.g. snd(x) = ? where x is a pair
translateAssignLhs (AssToPair (PairSnd e)) pst@(_,_,Registers (r:r':rs))
  = pst
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)]) -- move assignRHS into register r
    ==> translateExpr e                 --reference to pair stored in reg0
    ==> (++= [NullPointerRuntimeError])
    ==> (++= [LDR W AL r' (OffsetReg reg0 (IntOffset 8))])
    ==> storeIntoPair r r' 0


