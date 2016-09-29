module Util where

import Asm
import Grammar
import AllocateRegisters
import BuildFooter

import qualified Data.Map as Map
import Data.Maybe
import Data.List


{-
 -/////////////// Data Init ///////////////////////
-}

--containing the semi-translated prog, variable symbol table 
--and list of available registers
type ProgState = ([Instruction], SymbolTable, Registers) 
type SymbolTable = Map.Map (Ident, Int) (Register, Type)
--list of all registers
registers = Registers [TempGpReg x | x <- [1..]]
--list of registers to save on entry to subroutines
regsToSave = Registers [TempGpReg x | x <- [4..11]]
--list of the function argument registers
funcRegs = Registers [FuncArgReg x | x <- [1..]]
--the zero register
reg0 = TempGpReg 0


{-
 - ////////////// Helper Functions ///////////////////////
 -}

--functions for ProgState

--feed ProgState through functions
infixl 2 ==>
(==>) :: ProgState -> (ProgState -> ProgState) -> ProgState
(==>) pst f
  = f pst

--adds instructions to ProgState
infixl 4 ++=
(++=) :: ProgState -> [Instruction] -> ProgState
(is1,st,rs) ++= is2 = (is1++is2,st,rs)

--pointlessly does the same thing
infixl 4 =++
(=++) :: [Instruction] -> ProgState -> ProgState
is2 =++ (is1,st,rs) = (is1++is2,st,rs)

--do division
divide :: Register -> Register -> ProgState -> ProgState
divide rn rd pst
  = pst ++= [
          MOV No AL (FinalGpReg 0) (ShiftedRegister rn NOS),
          MOV No AL (FinalGpReg 1) (ShiftedRegister rd NOS),
          CMP AL (FinalGpReg 1) (ImmInt 0),
          DivideByZeroRuntimError,
          BL AL "__aeabi_idiv",
          MOV No AL rn (ShiftedRegister (FinalGpReg 0) NOS),
          MOV No AL rd (ShiftedRegister (FinalGpReg 1) NOS)]
          
modDest :: Register -> Register -> Register -> ProgState -> ProgState
modDest rdest rn rd pst
  = pst ++= [MOV No AL (FinalGpReg 0) (ShiftedRegister rn NOS),
             MOV No AL (FinalGpReg 1) (ShiftedRegister rd NOS),
             CMP AL (FinalGpReg 1) (ImmInt 0),
             DivideByZeroRuntimError,
             BL AL "__aeabi_idivmod",
             MOV No AL rdest (ShiftedRegister (FinalGpReg 1) NOS)]

--make a label
makeLabel :: String -> ProgState -> ProgState
makeLabel s
  = (++= [LABEL s])

--return from function, popping program counter and saved regs
returnFromFunc :: ProgState -> ProgState
returnFromFunc pst
  = pst
    ==> (++= [POP AL preservedRegisters])
    ==> (++= [POP AL (Registers [ProgramCounter])])


--load previously pushed parameters to local scope
getParams :: [Param] -> ProgState -> ProgState
getParams [] pst
 = pst
getParams (ParamDef t id:ps) pst@(is,st,Registers (r:rs))
 = pst
   ==> insertArgumentToSymbolTable id t
   ==> getParams ps 

--save registers on entering subroutine
saveRegs :: Registers -> ProgState -> ProgState
saveRegs rs
  = (++= [PUSH AL rs])

--restore registers on leaving subroutine
restoreRegs :: Registers -> ProgState -> ProgState
restoreRegs rs
  = (++= [POP AL rs])

--update the symbol table with latest version of (string, register) pairing
updateSymbolTableValues :: Ident -> Register -> ProgState -> ProgState
updateSymbolTableValues ident r pst@(is,st,rs)
  = (is,Map.update (\x -> Just (r, t)) (ident, findIdentScope ident st) st, rs)
  where
    (_, t) = lookupInSymbolTable ident pst

--looks up a value in the symbol table and puts it in reg0
lookupInSymbolTable :: Ident -> ProgState -> (Register, Type) 
lookupInSymbolTable ident (_, st, _)
     = fromJust $ Map.lookup (ident, findIdentScope ident st) st

--finds the first scope that an identifier belongs to.
findIdentScope :: Ident -> SymbolTable -> Int
findIdentScope ident st
  = head $ filter (\sc -> isJust (Map.lookup (ident,sc) st)) [0..]

--insert a new value into symbol table
insertSymbolTableValue :: Ident -> Type -> ProgState -> ProgState
insertSymbolTableValue ident t (is,st,Registers (r:rs))
  = (is, Map.insert (ident, 0) (r,t) st, Registers rs)

--inserts a special argument value int symbol table
insertArgumentToSymbolTable ::Ident -> Type -> ProgState -> ProgState
insertArgumentToSymbolTable ident t (is,st,Registers (TempGpReg val:rs))
  = (is, Map.insert (ident, 0) (FuncArgReg val,t) st, Registers rs)

--generate a unique label given a string and id number (line number)
generateLabel :: String -> Int -> ProgState -> ProgState
generateLabel s n
  = (++= [LABEL (s ++ show n)])

--jump to falseLabel if register == 0, to trueLabel otherwise
jumpOnReg :: Register -> String -> String -> ProgState -> ProgState
jumpOnReg r trueLabel falseLabel
  = (++= [CMP AL r (ImmInt 0),
          B NEq trueLabel,
          B AL falseLabel])

-- removes top register from the list of registers
removeTopRegister :: ProgState -> ProgState
removeTopRegister (is, st, Registers (r:rs))
  = (is ,st, Registers rs)

--adds a register to the list of free registers
putRegisterOnTop :: Register -> ProgState -> ProgState
putRegisterOnTop r (is, st, Registers rs)
  = (is, st, Registers (r:rs))

-- allocates memory and stores a list of registers onto the heap (array)
loadArray :: [Register] -> Int -> ProgState -> ProgState
loadArray rs size pst
  = pst 
    ==> allocateBlock size
    ==> loadArray' rs 0 
  where 
    loadArray' :: [Register] -> Int  -> ProgState -> ProgState
    loadArray' [] offset pst = pst
    loadArray' (r:rs) offset pst
      = pst
        ==> storeIntoArrayN r reg0 (offset-1)
        ==> loadArray' rs (offset+1)

-- allocates memory and stores a list of registers onto the heap (pairs)
loadPairs :: (Register, Register) -> ProgState -> ProgState
loadPairs (r1,r2) pst@(_,_,Registers (r:_))
  = pst
    ==> allocateBlock 3
    ==> (++= [MOV No AL r (ImmInt (-1))])
    ==> storeIntoPair r reg0 0
    ==> storeIntoPair r1 reg0 1
    ==> storeIntoPair r2 reg0 2

-- adds instruction to access array given at sourceReg offset by i
-- stores value in reg0
accessArrayO :: Register -> Register -> Type -> ProgState -> ProgState
accessArrayO sourceReg regArrayi t pst
  = pst ++= [LDR W AL reg0 (OffsetReg sourceReg 
                                  (RegOffset regArrayi (LSL shift)))]

  where
    (strType,shift) = case t of BaseType _  -> (UB,0)
                                _           -> (W, 2)

-- adds instruction to access array given at sourceReg offset by i
-- stores value in reg0
accessArrayN :: Register -> Int -> ProgState -> ProgState
accessArrayN sourceReg i pst
  = pst ++= [LDR W AL reg0 (OffsetReg sourceReg (IntOffset ((i+1)*4)))]

-- adds Instruction to store on an offset of a targetreg and
-- an offset.
storeIntoArrayO :: Register -> Register -> Register -> Type 
                                                    -> ProgState -> ProgState
storeIntoArrayO sourceReg targetReg regArrayi t pst
  = pst ++= [STR strType AL sourceReg (OffsetReg 
                                  targetReg (RegOffset regArrayi (LSL shift)))]
  where
    (strType, shift) = case t of BaseType _ -> (UB, 0)
                                 _          -> (W, 2)

-- adds Instruction to store on an offset given by a integer
-- off of a target reg
storeIntoArrayN :: Register -> Register -> Int -> ProgState -> ProgState
storeIntoArrayN sourceReg targetReg i pst
  = pst ++= [STR W AL sourceReg (OffsetReg targetReg (IntOffset ((i+1)*4)))]

-- adds Instruction to store on an offset given by a integer
-- off of a target reg
storeIntoPair :: Register -> Register -> Int -> ProgState -> ProgState
storeIntoPair sourceReg targetReg i pst
  = pst ++= [STR W AL sourceReg (OffsetReg targetReg (IntOffset (i*4)))]

-- loads Instruction to load an item from a pair given by 1 or 0
loadFromPair :: Register -> Int -> ProgState -> ProgState
loadFromPair sourceReg i pst
  = pst ++= [LDR W AL reg0 (OffsetReg sourceReg (IntOffset (i*4))),
             LDR W AL reg0 (OffsetReg reg0 (IntOffset 0))]

-- allocates one block of memory
-- returns reference to memory location in reg0
allocateBlock :: Int -> ProgState -> ProgState
allocateBlock i pst
  = pst ++= [ PUSH AL (RegisterRange (FinalGpReg 1) (FinalGpReg 12)),
              LDR W AL reg0 (LabAdd (show (i*4))), 
              BL AL "malloc",
              POP AL (RegisterRange (FinalGpReg 1) (FinalGpReg 12))]

