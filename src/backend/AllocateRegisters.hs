module AllocateRegisters where

import Asm

import Data.Maybe

type RegTable = [(Register, VarStoreLoc)]
data VarStoreLoc = RegStore Register       --Store in a register
                 | StackStore Int          --Store in the stack
                 deriving (Eq, Show)
instance Ord VarStoreLoc where
        RegStore r1 <= RegStore r2      = True
        StackStore i <= RegStore r      = True
        RegStore r <= StackStore i      = False
        StackStore i1 <= StackStore i2  = i1 <= i2

--Adds an offset to a variable location if it is in the stack
(+>) :: Int -> (Register, VarStoreLoc) -> (Register,VarStoreLoc)
(+>) _ (s,RegStore r) = (s,RegStore r)
(+>) o (s,StackStore r) = (s,StackStore (r+o))

--Returns true iff the StoreLoc is on the stack
isStackStore :: VarStoreLoc -> Bool
isStackStore (StackStore _) = True
isStackStore _            = False


--Adds an offset to all stack locations in a reg table
updateOffsets :: Int -> RegTable -> RegTable
updateOffsets offset
  = map (offset +>)

--Returns true if a register is already in use
regInUse :: RegTable -> Register -> Bool
regInUse tab reg
  = RegStore reg `elem` [dest | (_,dest) <- tab]

--List of general purpose registers
gpRegList :: [Register]
gpRegList = [FinalGpReg n | n <- [2..12]]

--Allocates registers for all functions. TOP LEVEL FUNCTION
--by mapping the function translator over the list of functions
allocateRegisters :: [[Instruction]] -> [[Instruction]]
allocateRegisters = map (allocateFuncRegisters [])

--Allocates registers for a single function
--by recursing on the list of instructions, and passing them to
--allocateInstructionRegisters
allocateFuncRegisters :: RegTable -> [Instruction] -> [Instruction]
allocateFuncRegisters table [lastInst]
  = [ADD No AL StackPointer StackPointer (ImmInt localVariablesOffset),
     lastInst]
     where
      localVariablesOffset = length (filter (\(_,s) 
                                  -> isStackStore s) table) * 4
allocateFuncRegisters tab (i:is)
  = newInst ++ allocateFuncRegisters newTab is
  where
      (newInst, newTab) = allocateInstructionRegisters i tab

--Allocates registers for a single assembly instruction
--by pattern matching on the instruction type
allocateInstructionRegisters :: Instruction -> RegTable 
                                          -> ([Instruction], RegTable)
allocateInstructionRegisters (MOV  a b r op2) t 
  = (prefix ++ [MOV a b rloc op2loc] ++ suffix, t'')
  where
      ((rloc, suffix), t'') = allocateStoreRegister r t'
      ((op2loc, prefix), t') = allocateOp2Register op2 t
allocateInstructionRegisters (MVN  a b r op2) t 
  = (prefix ++ [MVN a b rloc op2loc] ++ suffix, t'')
  where
      ((rloc, suffix), t'') = allocateStoreRegister r t'
      ((op2loc, prefix), t') = allocateOp2Register op2 t
allocateInstructionRegisters (LDM  a b r rs) t 
  = ([LDM a b r rs], t)
allocateInstructionRegisters (STM  a b r rs) t 
  = ([STM a b r rs], t)
allocateInstructionRegisters (PUSH a rs) t
  = ([PUSH a rs], t)
allocateInstructionRegisters (POP  a rs) t
  = ([POP a rs], t)
allocateInstructionRegisters (LDR  a b r or ) t
  = (prefix ++ [LDR a b rloc orloc] ++ suffix, t'')
  where
      ((rloc, suffix), t'') = allocateStoreRegister r t'
      ((orloc, prefix), t') = allocateOffsetRegister or t

allocateInstructionRegisters (STR  a b r or ) t 
  = (prefix ++ prefix2 ++ [STR a b rloc orloc], t'')
  where
      ((rloc, prefix2), t'') = allocateLoadRegister r t'
      ((orloc, prefix), t') = allocateOffsetRegister or t

allocateInstructionRegisters (CMP  a r op2     ) t
  = (prefix2 ++ prefix ++ [CMP a rloc op2loc], t'')
  where
      ((rloc, prefix2), t'') = allocateLoadRegister r t'
      ((op2loc, prefix), t') = allocateOp2Register op2 t
allocateInstructionRegisters (CMN  a r op2     ) t
  = (prefix2 ++ prefix ++ [CMN a rloc op2loc], t'')
  where
      ((rloc, prefix2), t'') = allocateLoadRegister r t'
      ((op2loc, prefix), t') = allocateOp2Register op2 t
allocateInstructionRegisters (TST  a r op2     ) t
  = (prefix2 ++ prefix ++ [TST a rloc op2loc], t'')
  where
      ((rloc, prefix2), t'') = allocateLoadRegister r t'
      ((op2loc, prefix), t') = allocateOp2Register op2 t
allocateInstructionRegisters (TEQ  a r op2     ) t
  = (prefix2 ++ prefix ++ [TEQ a rloc op2loc], t'')
  where
      ((rloc, prefix2), t'') = allocateLoadRegister r t'
      ((op2loc, prefix), t') = allocateOp2Register op2 t
allocateInstructionRegisters (ADD  a b r r1 op2) t 
  = (prefix2 ++ prefix ++ [ADD a b rdLoc rLoc op2loc] ++ suffix, t'')
  where
      ((rdLoc, suffix), t''') = allocateStoreRegister r t''
      ((rLoc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (MUL a rdl rdh rn rm) t
  = (prefix2 ++ prefix ++ [MUL a rdl rdh rnloc rmloc], t'')
  where
      ((rnloc, prefix2), t'') = allocateLoadRegister rn t'
      ((rmloc, prefix), t') = allocateLoadRegister rm t
allocateInstructionRegisters (SUB  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [SUB a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (RSB  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [RSB a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (SUBW a b r r1 d  ) t = undefined
allocateInstructionRegisters (AND  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [AND a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (ORR  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [ORR a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (EOR  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [EOR a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (BIC  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [BIC a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters (ORN  a b r r1 op2) t
  = (prefix2 ++ prefix ++ [ORN a b rdloc rloc op2loc] ++ suffix, t'')
  where
      ((rdloc, suffix), t''') = allocateStoreRegister r t''
      ((rloc, prefix), t'') = allocateLoadRegister r1 t'
      ((op2loc, prefix2), t') = allocateOp2Register op2 t
allocateInstructionRegisters i t
  = ([i], t)

--Allocate registers for an Operand2, as well as returning 
--the required load instructions
allocateOp2Register :: Operand2 -> RegTable 
                            -> ((Operand2, [Instruction]), RegTable)
allocateOp2Register (ShiftedRegister r s) tab
  | null stackInst = ((ShiftedRegister r' s, []), newTable)
  | otherwise      = ((ShiftedRegister (FinalGpReg 1) s, stackInst), newTable)
  where
      (newStore, newTable, _) = allocateRegister r tab
      RegStore r' = newStore
      StackStore stackLoc = newStore
      stackInst = concat [[ LDR W AL (FinalGpReg 1) (OffsetReg StackPointer 
                                (IntOffset stackLoc))] | isStackStore newStore]
allocateOp2Register op2 tab
  = ((op2, []), tab)

--Allocate registers for an indexed offset
allocateOffsetRegister :: Operand2Mem -> RegTable 
                                    -> ((Operand2Mem, [Instruction]), RegTable)
allocateOffsetRegister (OffsetReg r (RegOffset r1 i)) tab
  = ((OffsetReg rbloc (RegOffset riloc i), prefix ++ prefix2), t'')
  where 
      ((rbloc, prefix), t'') = allocateLoadRegister r t'
      ((riloc, prefix2), t') = allocateIndexRegister r1 tab
allocateOffsetRegister (OffsetReg r (IntOffset i)) tab
  = ((OffsetReg rbloc (IntOffset i), prefix), t')
  where 
      ((rbloc, prefix), t') = allocateLoadRegister r tab
allocateOffsetRegister (LabAdd str) tab
  = ((LabAdd str, []), tab) 
allocateOffsetRegister (IntAdd n) tab 
  = ((IntAdd n, []), tab) 

--Allocates a register and returns push instruction for a written register
allocateIndexRegister :: Register -> RegTable 
                                  -> ((Register, [Instruction]), RegTable)
allocateIndexRegister r tab
  | isStackStore stLoc      = ((FinalGpReg 1, stackInst), newTab)
  | otherwise               = ((r', []), newTab)
  where
      (stLoc, newTab, _) = allocateRegister r tab
      StackStore stackLoc = stLoc
      stackInst = [STR W AL (FinalGpReg 1) (OffsetReg StackPointer 
                                                    (IntOffset stackLoc))]
      RegStore r' = stLoc

--Allocates a register and returns pop instruction for a consumed register
allocateLoadRegister :: Register -> RegTable 
                                    -> ((Register, [Instruction]), RegTable)
allocateLoadRegister r tab
  | isStackStore ldLoc      = ((FinalGpReg 1, stackInst), newTab)
  | otherwise               = ((r', []), newTab)
  where
      (ldLoc, newTab, _) = allocateRegister r tab
      StackStore stackLoc = ldLoc
      stackInst = [LDR W AL (FinalGpReg 1) (OffsetReg StackPointer 
                                                    (IntOffset stackLoc))]
      RegStore r' = ldLoc

--Allocates a register and returns push instruction for a written register
allocateStoreRegister :: Register -> RegTable 
                                    -> ((Register, [Instruction]), RegTable)
allocateStoreRegister r@(FuncArgReg i) tab
  | isStackStore stLoc      = ((FinalGpReg 0, stackInst), newTab)
  | otherwise               = ((r', []), newTab)
  where
      (stLoc, newTab, _) = allocateRegister r tab
      StackStore stackLoc = stLoc
      stackInst = [STR W AL (FinalGpReg 0) (OffsetReg StackPointer 
                                                      (IntOffset stackLoc))]
      RegStore r' = stLoc
allocateStoreRegister r tab
  | isStackStore stLoc      = ((FinalGpReg 0, stackInst), newTab)
  | otherwise               = ((r', []), newTab)
  where
      (stLoc, newTab, decStack) = allocateRegister r tab
      StackStore stackLoc = stLoc
      stackInst = [SUB No AL StackPointer StackPointer (ImmInt 4),
                   STR W AL (FinalGpReg 0) (OffsetReg StackPointer 
                                                      (IntOffset stackLoc))]
      RegStore r' = stLoc

--Allocates registers or stack for a single register access;
--either a free register from freeRegisters or a stack location
allocateRegister :: Register -> RegTable -> (VarStoreLoc, RegTable, Bool)
allocateRegister reg@(TempGpReg _) table
  | reg == TempGpReg 0 = (RegStore (FinalGpReg 0), table, False)
  | isNothing tableLookup && (freeRegisters /= [])
        = (targetRegister, (reg, targetRegister) : table, False)
  | isNothing tableLookup
        = (StackStore 0, (reg, StackStore 0) : updateOffsets 4 table, True)
  | otherwise
        = (fromJust tableLookup, table, False)
  where
      tableLookup = lookup reg table
      targetRegister = RegStore (head freeRegisters)
      freeRegisters = filter (not . regInUse table) gpRegList
allocateRegister reg@(FuncArgReg i) table
  = (StackStore (baseArgOffset + 4 * i), table, False)
  where
      pushedRegsOffset = 11 * 4  --The space taken up by pushed registers
      localVariablesOffset = length (filter (\(_,s) -> isStackStore s) table)*4
      baseArgOffset = pushedRegsOffset + localVariablesOffset
allocateRegister reg tab
  = (RegStore reg, tab, False)
