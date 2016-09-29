module Peephole where

import Asm


--Perform peephole analysis on generated assembly
peepOptimise :: [Instruction] -> [Instruction]
--Base and no-improvement cases
peepOptimise []
  = []

--Collapses move "a->b, b->a" into a single move "a->b"
peepOptimise (i1@(MOV No AL reg1a (ShiftedRegister reg1b NOS))
             :i2@(MOV No AL reg2a (ShiftedRegister reg2b NOS)) : is)
  | reg1a == reg2b && reg1b == reg2a    = (MOV No AL reg1a (ShiftedRegister reg1b NOS)) : peepOptimise is
  | otherwise                           = i1 : peepOptimise (i2:is)

--Removes moves from register into same register
peepOptimise (i@(MOV No AL reg1a (ShiftedRegister reg1b NOS)) : is)
  | reg1a == reg1b    = peepOptimise is
  | otherwise         = i : peepOptimise is

--Removes +/- 0 instructions, as they have no effect
peepOptimise (i1@(ADD No _ reg1a reg2a (ImmInt 0)) : is)
  = peepOptimise is

peepOptimise (i1@(SUB No _ reg1a reg2a (ImmInt 0)) : is)
  = peepOptimise is

peepOptimise (i1@(ADDW No _ reg1a reg2a 0) : is)
  = peepOptimise is

peepOptimise (i1@(SUBW No _ reg1a reg2a 0) : is)
  = peepOptimise is

--Removes bitwise OR and XOR with 0 instructions
peepOptimise (i1@(ORR No _ reg1a reg2a (ImmInt 0)) : is)
  = peepOptimise is

peepOptimise (i1@(EOR No _ reg1a reg2a (ImmInt 0)) : is)
  = peepOptimise is

--Simplifies successive addition and subtraction of immediates
peepOptimise (i1@(ADD No AL reg1a reg1b (ImmInt n1)) 
             :i2@(ADD No AL reg2a reg2b (ImmInt n2)): is)
  | reg1a == reg2a && reg2a == reg2b
    = (ADD No AL reg1a reg1b (ImmInt (n1+n2)) : is)

peepOptimise (i1@(ADD No AL reg1a reg1b (ImmInt n1)) 
             :i2@(SUB No AL reg2a reg2b (ImmInt n2)): is)
  | reg1a == reg2a && reg2a == reg2b
    = (ADD No AL reg1a reg1b (ImmInt (n1-n2)) : is)

peepOptimise (i1@(SUB No AL reg1a reg1b (ImmInt n1)) 
             :i2@(ADD No AL reg2a reg2b (ImmInt n2)): is)
  | reg1a == reg2a && reg2a == reg2b
    = (ADD No AL reg1a reg1b (ImmInt (n2-n1)) : is)

peepOptimise (i1@(SUB No AL reg1a reg1b (ImmInt n1)) 
             :i2@(SUB No AL reg2a reg2b (ImmInt n2)): is)
  | reg1a == reg2a && reg2a == reg2b
    = (SUB No AL reg1a reg1b (ImmInt (n1+n2)) : is)

--Simplifies subtract followed by compare
peepOptimise (i1@(SUB _ c1 reg1a reg1b (op2a)) 
             :i2@(CMP c2 reg2a (op2b)): is)
  | c1 == c2 && reg1b == reg2a && op2a == op2b
    = (SUB Yes c1 reg1a reg1b (op2a) : is)

--Simplifies addition followed by compare negative
peepOptimise (i1@(ADD _ c1 reg1a reg1b (op2a)) 
             :i2@(CMN c2 reg2a (op2b)): is)
  | c1 == c2 && reg1b == reg2a && op2a == op2b
    = (ADD Yes c1 reg1a reg1b (op2a) : is)

--Simplifies repeated instructions
peepOptimise (i1:i2:is)
  | i1 == i2    = peepOptimise (i1:is)
  | otherwise   = i1 : peepOptimise (i2:is)

--No-improvement case
peepOptimise (i:is)
  = i : peepOptimise is
