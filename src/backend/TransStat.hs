module TransStat where


-- Imports.
import Asm
import Grammar
import AllocateRegisters
import Util
import TransExpr
import TransAssign
import qualified Data.Map as Map
import Data.Maybe



--Translates a statement
--from our internal ASM to the intermediate assembler code
-- @Param: Statement from Tree, ProgramState
-- @Return Program State after the execution of a statement
--
--Program state includes:
--  A list of instructions,
--  SymbolTable,
--  A list of free registers

--Translates a program
translateStat :: Stat -> ProgState -> ProgState
translateStat (StatDeclare _ t ident arhs) pst@(_,_,Registers (r:rs))
  = pst
    ==> translateAssignRhs arhs
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)])
    ==> insertSymbolTableValue ident t

--Translates a struct declare
--Just adds all attributes int othe symboltable
translateStat (StatStructDeclare _ stype@(StructType name) id) pst@(_, st, Registers (r:_))
  = pst
    ==> allocateBlock (size+1)
    ==> (++= [MOV No AL r (ShiftedRegister reg0 NOS)])
    ==> removeTopRegister
    ==> (++= [LDR W AL reg0 (IntAdd size), 
                STR W AL r (OffsetReg reg0 (IntOffset 0))])
    ==> addStructsAndAllocate stAttrs 4 r
    ==> (++= [MOV No AL reg0 (ShiftedRegister r NOS)])
    ==> insertSymbolTableValue (StructBlock id stAttrs) stype
  where
    stList  = map fst $ map fst $ Map.toList st
    stAttrs = head [list | StructBlock x list <- stList, x == Id name]
    size    = length stAttrs
    
    addStructsAndAllocate :: [(Ident,Type)] -> Int -> Register -> ProgState -> ProgState
    addStructsAndAllocate [] _ _ pst = pst 
    addStructsAndAllocate ((attrId,attrT):rest) offset structReg pst@(_,_,Registers (r:rs))
      = addStructsAndAllocate rest (offset + 4) structReg newPst
      where 
        newPst = pst
            ==> allocateBlock 1
            ==> (++= [LDR W AL r (IntAdd 0),
                      STR W AL r (OffsetReg reg0 (IntOffset 0)),
                      STR W AL reg0 (OffsetReg structReg (IntOffset offset))])
            ==> putRegisterOnTop structReg
            ==> insertSymbolTableValue (SElemId id attrId) attrT
                        
-- Translates Assignments recursively into assembler
translateStat (StatAssign _ alhs arhs) pst
  = pst
    ==> translateAssignRhs arhs 
    ==> translateAssignLhs alhs 

-- Translates ReadStatements recursively
-- Checks if is int or char and then 
-- adds the approprite label
translateStat (TypedStatRead _ t alhs) pst
  = pst 
    ==> (++= case t of
          BaseType IntType  -> [ReadInt]
          BaseType CharType -> [ReadChar])
    ==> translateAssignLhs alhs -- reg to save it in reg0

-- Translates Free Statements
-- Adds the psudoInstruction for the 3rd pass later on
translateStat (StatFree _ e) pst
  = pst
    ==> translateExpr e
    ==> (++= [Free])
    
--Translates Return Statements
translateStat (StatReturn _ e) pst
  = pst
    ==> translateExpr e
    ==> returnFromFunc

--Translates and exit statement
translateStat (StatExit _ e) pst 
  = translateExpr e pst ++= [BL AL "exit"]

--Trasnaltes a pritn statement,
--Adds the appropriate print psudo-instruction
--For the footer pass later on
translateStat (TypedStatPrint _ t e) pst
  = pst     
    ==> translateExpr e
    ==> (++= case t of 
      BaseType IntType    -> [PrintInt]
      BaseType CharType   -> [PrintChar]
      BaseType StringType -> [PrintString]
      BaseType BoolType   -> [PrintBool]
      _                   -> [PrintPointer])

-- Translaates a print ln as above
translateStat (TypedStatPrintln _ t e) pst
  = pst
    ==> translateExpr e
    ==> (++= case t of 
      BaseType IntType    -> [PrintInt, PrintLn] 
      BaseType CharType   -> [PrintChar, PrintLn] 
      BaseType StringType -> [PrintString, PrintLn]
      BaseType BoolType   -> [PrintBool, PrintLn] 
      _                   -> [PrintPointer, PrintLn])
    

--Translates an if statment
--Adds assembler instructions
translateStat (StatIf (n,_) e s1 s2) pst
  = pst -- Branching
   ==> translateExpr e
   ==> jumpOnReg reg0 trueLabel falseLabel
   ==> generateLabel "then" n
   ==> translateStat s1
   ==> (++= [B AL endLabel])
   ==> generateLabel "else" n
   ==> translateStat s2
   ==> generateLabel "end" n
   where
    trueLabel  = "then" ++ show n
    falseLabel = "else" ++ show n
    endLabel   = "end" ++ show n

--translate while statement
translateStat (StatWhile (n,_) e s) pst
  = pst -- Branching
    ==> generateLabel "while" n
    ==> translateExpr e
    ==> (++= [CMP AL reg0 (ImmInt 0), B EQu endLabel])
    ==> translateStat s
    ==> (++= [B AL whileLabel])
    ==> generateLabel "end" n
    where
      whileLabel = "while" ++ show n
      endLabel   = "end" ++ show n

--Translates a begin statement
translateStat (StatBegin _ s) pst@(is,st,rs)
  = (is',st,rs)
  where
    (is',_,_) = translateStat s pst

--Translates a composition of statments 
--by calling each statment recursviely
translateStat (StatCompose _ s1 s2) pst
  = pst 
    ==> translateStat s1
    ==> translateStat s2

--Translates a new scoping level
--Adds one to each scope in the symbol tabel 
--before recusively translating statmetns
--then reverts scoping back to original
translateStat (StatNewScope _ s) pst@(is ,st ,rs )
  = (is ,Map.mapKeys (\(r, sc) ->(r,sc+1)) st , rs)
    ==> translateStat s
    ==> (\(i', s', r') -> (i' ,Map.mapKeys (\(r'', sc) -> (r'',sc-1)) s', r'))

-- Translates a skip statement,
-- Nothing in the state of the program changes
translateStat s pst
  = pst -- Just keep the original statement


