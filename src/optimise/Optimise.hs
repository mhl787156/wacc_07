module Optimise where

import Asm

import Peephole

optimise :: [Instruction] -> AsmProgram
optimise is
  = AsmProgram (peepOptimise is)
