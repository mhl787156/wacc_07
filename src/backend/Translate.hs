module Translate where

import Asm
import Grammar
import AllocateRegisters
import Util
import TransStat
import TransExpr
import TransAssign
import BuildFooter

import Optimise


import qualified Data.Map as Map
import Data.Maybe

{-
 -////////// AST TRANSLATION ////////////////////////
-}

translateAndOptimise :: Program -> AsmProgram
translateAndOptimise
  = optimise . translateProg

-- translate a program into a list of ARM commands
translateProg :: Program -> [Instruction]
translateProg (Program imps structs fs s) 
    = wholeProg

  where
    pst = ([],Map.fromList [],registers)
    allFuncs = translateFuncs fs pst ++ [mainFunc] 
    mainFunc = LABEL "main" : completeMain (translateStat s pst)

    wholeProg = buildAndAddFooter (concat (allocateRegisters allFuncs) ++ [Header ".ltorg"])

--Add beginning and ending parts to main function
completeMain :: ProgState -> [Instruction]
completeMain (is, _, _)
  = [PUSH AL (Registers [ReturnPointer])] ++
    is ++
    [MOV No AL (FinalGpReg 0) (ImmInt 0),
    POP AL (Registers [ProgramCounter])]

--translate list of functions
translateFuncs :: [Func] -> ProgState -> [[Instruction]]
translateFuncs (f:fs) pst
  = (let (is,_,_) = pst ==> translateFunc f in is) : translateFuncs fs pst
translateFuncs _ pst
  = []


--translate individual function
translateFunc :: Func -> ProgState -> ProgState
translateFunc (FuncDef _ (Id id) (ParamList ps) s) (is,_,rs)
  = (is',newST,rs)
  where
    newST = Map.fromList []
    (is',_,_) = (is,newST,rs) 
                ==> makeLabel ("func_" ++ id)                   --name function
                ==> (++= [PUSH AL (Registers [ReturnPointer])]) --push link reg
                ==> (++= [PUSH AL preservedRegisters])
                ==> getParams ps                                --get params
                ==> translateStat s                             --trans body
                ==> (++= [POP AL preservedRegisters])
                ==> (++= [POP AL (Registers [ProgramCounter])]) --restore link

translateStructs :: [Struct] -> ProgState -> ProgState
translateStructs [] pst = pst
translateStructs (StructDef _ t@(StructType name) ss : sts) pst@(_,st,_)
  = pst 
    ==> insertSymbolTableValue (StructBlock (Id name) structAttrSt) t
    ==> translateStructs sts
  where
    structAttrSt = translateStructStat ss []


translateStructStat :: StructStat -> IdentTypeMap -> IdentTypeMap
translateStructStat StructEmpty st
  = st
translateStructStat (StructDeclare t id) st
  = (id, t) : st
translateStructStat (StructStatCompose ss1 ss2) st 
  = translateStructStat ss1 st1
  where 
    st1 = translateStructStat ss1 st

type IdentTypeMap = [(Ident,Type)]
