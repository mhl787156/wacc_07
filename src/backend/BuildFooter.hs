module BuildFooter where

import Asm 
import Grammar

import Data.List



{-
 -/////////////// 3rd Pass ///////////////////////
 -///Building Footer of common Functions//////////
-}


--scans the assembly produced for strings and language function
--calls (eg. print, !) and puts them in a footer
buildAndAddFooter :: [Instruction] -> [Instruction]
buildAndAddFooter is
  = [Header ".text"] ++
    [Header ".global main"] ++
    p ++
    (concat . nub) fs' ++
    [Header ".data"] ++
    (concat . nub) d'

  where
    (is',fs',d',p) = buildAndAddFooter' (is,[],[],[]) 0
    buildAndAddFooter' :: EditProgram -> Int -> EditProgram
    buildAndAddFooter' ([],fs,d,p) n = ([], fs, d, p)
    buildAndAddFooter' (i:is,fs,d,p) n
      = case i of
          -- If a print int is detected, the relevant code will
          -- be added to to the program footer
          -- This includes the format specifier string which is added 
          -- to your data
          PrintInt
            -> buildAndAddFooter' (is,
                                   printType i:fs,
                                   [LABEL "int_format_specifier",
                                       DataDeclare (WORD 2),
                                       DataDeclare (ASCII "%d")]:d,
                                   p++[BL AL "p_print_int"]) n
          -- If a print string is detected, the string printing code
          -- will be added to the footer
          -- This is added also to print out any error messages
          -- This includes the format specifier string which is added 
          -- to your data
          PrintString
            -> buildAndAddFooter' (is,
                                   printType i:fs,
                                   [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]:d,
                                   p++[BL AL "p_print_string"]) n
          -- If a print char is detected, the releveant code will
          -- be added to the program footer.
          -- This includes the format specifier string which is added 
          -- to your data
          PrintChar
            -> buildAndAddFooter' (is,
                                   printType i:fs,
                                   [LABEL "char_format_specifier",
                                       DataDeclare (WORD 2),
                                       DataDeclare (ASCII "%c")]:d,
                                   p++[BL AL "p_print_char"]) n
          -- If a print line is detected, the releveant code will
          -- be added to the program footer.
          -- This includes the format specifier string which is added 
          -- to your data
          PrintLn
            -> buildAndAddFooter' (is,
                                   printType i:fs,
                                   [LABEL "ln_format_specifier",
                                       DataDeclare (WORD 0),
                                       DataDeclare (ASCII "")]:d,
                                   p++[BL AL "p_print_ln"]) n
          -- If a print pointer is detected, the releveant code will
          -- be added to the program footer.
          -- This includes the format specifier string which is added 
          -- to your data
          PrintPointer
            -> buildAndAddFooter' (is,
                                   printType i:fs,
                                   [LABEL "pointer_format_specifier",
                                       DataDeclare (WORD 2),
                                       DataDeclare (ASCII "%p")]:d,
                                   p++[BL AL "p_print_pointer"]) n
          -- If a print bool is detected, the relevant code will
          -- be added to the program footer.
          -- This includes the format specifier string which is added 
          -- to your data
          -- In the print bool, we treat it as printing the string
          -- true or false
          PrintBool
            -> buildAndAddFooter' (is,
                                   printType PrintString:fs,
                                   [[LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]] ++
                                       [[LABEL "true_str",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "true"),
                                       LABEL "false_str",
                                       DataDeclare (WORD 5),
                                       DataDeclare (ASCII "false")]]++d,
                                   p++[CMP AL (FinalGpReg 0) (ImmInt 0),
                                       LDR W NEq (FinalGpReg 0) (LabAdd "true_str"),
                                       LDR W EQu (FinalGpReg 0) (LabAdd "false_str"),
                                       BL AL "p_print_string"]) n
          -- If a read integer is detected, the relevant code will
          -- be added to the program footer.
          -- This includes the format specifier string which is added 
          -- to your data
          ReadInt
            -> buildAndAddFooter' (is,
                                   readType i:fs,
                                   [[LABEL "int_format_specifier",
                                       DataDeclare (WORD 2),
                                       DataDeclare (ASCII "%d")]] ++
                                       [[LABEL "intRead",
                                       DataDeclare (WORD 0),
                                       LABEL "intRead_addr",
                                       DataDeclare (WORDSTRING "intRead")]]++d,
                                   p++[BL AL "p_read_int"]) n
          -- If a read char is detected, the relevant code will
          -- be added to the program footer.
          -- This includes the format specifier string which is added 
          -- to your data
          ReadChar
            -> buildAndAddFooter' (is,
                                   readType i:fs,
                                   [[LABEL "char_format_specifier_read",
                                       DataDeclare (WORD 2),
                                       DataDeclare (ASCII " %c")]] ++
                                       [[LABEL "charRead",
                                       DataDeclare (WORD 0),
                                       LABEL "charRead_addr",
                                       DataDeclare (WORDSTRING "charRead")]]++d,
                                   p++[BL AL "p_read_char"]) n
          -- If a user string is detected, the string
          -- will be stored in the data section of the footer
          -- with the WORD length and ASCII string.
          (StoreString r s)
            -> buildAndAddFooter' (is,
                                   fs,
                                   [LABEL ("str_" ++ show n),
                                       DataDeclare (WORD ((length s) + 4)),
                                       DataDeclare (ASCII s)]:d,
                                   p++[LDR W AL (FinalGpReg 0) (LabAdd ("str_" ++ show n))]) (n+1)
          --Runtime error code gets added if there is a possibility
          --of out of bounds, i.e. for all array accesses.
          OutOfBoundsRuntimeError
            -> buildAndAddFooter' (is,
                                   throwRuntimeError:(printType PrintString:(checkArrayBounds:fs)),
                                   [[LABEL "neg_array_out_of_bounds_error",
                                       DataDeclare (WORD 44),
                                       DataDeclare (ASCII "ArrayIndexOutOfBoundsError: negative index\\n\0"),
                                    LABEL "big_array_out_of_bounds_error",
                                       DataDeclare (WORD 45),
                                       DataDeclare (ASCII "ArrayIndexOutOfBoundsError: index too large\\n\0")],
                                    [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]]++d,
                                   p++[BL AL "p_check_array_bounds"]) n 
          --Runtime Overflow Error code added in all cases where
          --there is a possibility of overflow or underflow
          IntegerOverflowUnderflowRuntimeError
            -> buildAndAddFooter' (is,
                                   throwRuntimeError:(printType PrintString:(checkOverflow:fs)),
                                   [[LABEL "int_overflow_error",
                                       DataDeclare (WORD 82),
                                       DataDeclare (ASCII "OverflowError: the result is too small/large to store in a 4-byte signed-int\\n\0")],
                                    [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]]++d,
                                   p++[BL AL "p_check_integer_overflow"]) n
          --Runtime Overflow Error code added in all cases where
          --there is a possibility of overflow or underflow
          --Special case for multiplication
          MulOverflowUnderflowRuntimeError
            -> buildAndAddFooter' (is,
                                   throwRuntimeError:(printType PrintString:(checkMulOverflow:fs)),
                                   [[LABEL "int_overflow_error",
                                       DataDeclare (WORD 82),
                                       DataDeclare (ASCII "OverflowError: the result is too small/large to store in a 4-byte signed-int\\n\0")],
                                    [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]]++d,
                                   p++[BL NEq "p_throw_overflow_error"]) n
          --Runtime Divide by zero code added in all cases where 
          --there is a possibility of a divide by zero
          DivideByZeroRuntimError
            -> buildAndAddFooter' (is,
                                   throwRuntimeError:(printType PrintString:(checkDivideByZero:fs)),
                                   [[LABEL "divide_by_zero_error",
                                       DataDeclare (WORD 45),
                                       DataDeclare (ASCII "DivideByZeroError: divide or modulo by zero\\n\0")],
                                    [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]]++d,
                                   p++[BL AL "p_check_divide_by_zero"]) n
          --Runtime null pointer dereference code added in all cases wjere
          --we deal with a pointer
          NullPointerRuntimeError
            -> buildAndAddFooter' (is,
                                   throwRuntimeError:(printType PrintString:(checkNullPointer:fs)),
                                   [[LABEL "null_pointer_error",
                                      DataDeclare (WORD 50),
                                      DataDeclare (ASCII "NullReferenceError: dereference a null reference\\n\0")],
                                    [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]]++d,
                                   p++[BL AL "p_check_null_pointer"]) n
          --Code to deal with the freeing of any generic array or pointer
          --Only added if a free instruction is used in the code.
          Free
            -> buildAndAddFooter' (is,
                                   throwRuntimeError:(printType PrintString:(checkNullPointer:(freePointer:fs))),
                                   [[LABEL "null_pointer_error",
                                      DataDeclare (WORD 50),
                                      DataDeclare (ASCII "NullReferenceError: dereference a null reference\\n\0")],
                                    [LABEL "string_format_specifier",
                                       DataDeclare (WORD 4),
                                       DataDeclare (ASCII "%.*s")]]++d,
                                   p++[BL AL "p_free_pointer"]) n
          --Base Case, should never be accessed.
          _
            -> buildAndAddFooter' (is,
                                   fs,
                                   d,
                                   p++[i]) n


type EditProgram = (ARMProgram, AddedFunctions, DataSection, ARMProgram)
type DataSection = [[Instruction]]
type AddedFunctions = [[Instruction]]
type ARMProgram = [Instruction]

{-
 - ////////////////// Instructions to be added into the footer ///////////////////
 -}

preservedRegisters :: Registers
preservedRegisters = RegisterRange (FinalGpReg 2) (FinalGpReg 12)


--print function to be added at the end of the program
--depending on the type of the instruction, it will use
--a slightly different print method.
printType :: Instruction -> [Instruction]
printType i 
  = [LABEL fn,
    PUSH AL (Registers [ReturnPointer]),
    PUSH AL preservedRegisters]
    ++ gt ++ ad ++
    [LDR W AL (FinalGpReg 0) (LabAdd sp), 
    ADD No AL (FinalGpReg 0) (FinalGpReg 0) (ImmInt 4),
    BL AL (fst pr),
    MOV No AL (FinalGpReg 0) (ImmInt 0),
    BL AL "fflush",
    POP AL preservedRegisters,
    POP AL (Registers [ProgramCounter])]
    where 
      (fn,sp,gt,ad) = case i of PrintInt     -> ("p_print_int",     "int_format_specifier", mv,[])
                                PrintChar    -> ("p_print_char",    "char_format_specifier", mv,[])
                                PrintString  -> ("p_print_string",  "string_format_specifier", ld,add)
                                PrintPointer -> ("p_print_pointer", "pointer_format_specifier", mv,[])
                                PrintLn      -> ("p_print_ln",      "ln_format_specifier",[],[])
                                _            -> ("blahblahblahblah","RARRRRRRRRR",[],[])
      pr = case i of PrintLn      -> ("puts",AL)
                     _            -> ("printf", AL)
      mv = [MOV No AL (FinalGpReg 1) (ShiftedRegister (FinalGpReg 0) NOS)]
      ld = [LDR W AL (FinalGpReg 1) (OffsetReg (FinalGpReg 0) (IntOffset 0))]
      add = [ADD No AL (FinalGpReg 2) (FinalGpReg 0) (ImmInt 4)]
 

--read function to be added at the end of the program
--depending on the type of the instruction, it will use
--a slightly different read  method.
readType :: Instruction -> [Instruction]
readType i
  = [LABEL dt1,
    PUSH AL (Registers [ReturnPointer]),
    PUSH AL preservedRegisters,
    LDR W AL (FinalGpReg 1) (LabAdd dt3),
    LDR W AL (FinalGpReg 0) (LabAdd dt2), 
    ADD No AL (FinalGpReg 0) (FinalGpReg 0) (ImmInt 4),
    BL AL "scanf",
    LDR W AL (FinalGpReg 0) (LabAdd dt3),
    LDR W AL (FinalGpReg 0) (OffsetReg (FinalGpReg 0) (IntOffset 0)),
    POP AL preservedRegisters,
    POP AL (Registers [ProgramCounter])]
  where 
      (dt1,dt2,dt3,dt4) = case i of  ReadInt     -> ("p_read_int", 
                                                     "int_format_specifier", 
                                                     "intRead", "intRead_addr")
                                     ReadChar    -> ("p_read_char",
                                                     "char_format_specifier_read",
                                                     "charRead", 
                                                     "charRead_addr")
                                     _           -> ("SOMTHING WRONG", 
                                                     "SOMETHING WRONG", [],[])

--Adds assembly to check for null pointer dereferencing
--If there is, a runtime error is then thrown.
checkNullPointer :: [Instruction]
checkNullPointer 
  = [ LABEL "p_check_null_pointer",
      PUSH AL (Registers [ReturnPointer]),
      CMP AL (FinalGpReg 0) (ImmInt 0),
      LDR W EQu (FinalGpReg 0) (LabAdd "null_pointer_error"),
      BL EQu "p_throw_runtime_error",
      POP AL (Registers [ProgramCounter]) ]
      

--Adds assembly to help free allocated blocks of memory on the heap
--runs the checkNullPointer function, and will runtime error
--if attempt to double free
freePointer :: [Instruction]
freePointer
  = [ LABEL "p_free_pointer",
      PUSH AL (Registers [ReturnPointer]),
      PUSH AL (RegisterRange (FinalGpReg 1) (FinalGpReg 12)),
      BL AL "p_check_null_pointer",
      LDR W AL (FinalGpReg 1) (OffsetReg (FinalGpReg 0) (IntOffset 0)),
      CMP AL (FinalGpReg 1) (ImmInt (-1)),
      B NEq "p_free_not_pair",

      MOV No AL (FinalGpReg 4) (ShiftedRegister (FinalGpReg 0) NOS),
      LDR W AL (FinalGpReg 0) (OffsetReg (FinalGpReg 4) (IntOffset 4)),
      BL AL "p_check_null_pointer",
      BL AL "free",
      LDR W AL (FinalGpReg 0) (OffsetReg (FinalGpReg 4) (IntOffset 8)),
      BL AL "p_check_null_pointer",
      BL AL "free",
      MOV No AL (FinalGpReg 0) (ShiftedRegister (FinalGpReg 4) NOS),

      LABEL "p_free_not_pair",
      BL AL "free",
      POP AL (RegisterRange (FinalGpReg 1) (FinalGpReg 12)),
      POP AL (Registers [ProgramCounter]) ]

--Adds assembly code to check array bounds 
--during any given array access
--will runtime error if out of bounds
checkArrayBounds :: [Instruction]
checkArrayBounds 
  = [ LABEL "p_check_array_bounds",
      PUSH AL (Registers [ReturnPointer]),
      CMP AL (FinalGpReg 0) (ImmInt 0),
      LDR W LThan (FinalGpReg 0) (LabAdd "neg_array_out_of_bounds_error"),
      BL LThan "p_throw_runtime_error",
      LDR W AL (FinalGpReg 1) (OffsetReg (FinalGpReg 1) (IntOffset 0)),
      CMP AL (FinalGpReg 0) (ShiftedRegister (FinalGpReg 1) NOS),
      LDR W CS (FinalGpReg 0) (LabAdd "big_array_out_of_bounds_error"),
      BL CS "p_throw_runtime_error",
      POP AL (Registers [ProgramCounter]) ]

--Adds assembly code to check for overflow 
--during most arithmetic instructions
--will runtime error if there is overflow
checkOverflow :: [Instruction]
checkOverflow 
  = [ LABEL "p_check_integer_overflow",
      PUSH AL (Registers [ReturnPointer]),
      BL VS "p_throw_overflow_error_asdfghjkl",
      POP AL (Registers [ProgramCounter]),
      LABEL "p_throw_overflow_error_asdfghjkl",
      LDR W AL (FinalGpReg 0) (LabAdd "int_overflow_error"),
      BL AL "p_throw_runtime_error"]

--Adds assembly code to check for overflow
--This is the special case for multiplication
--where the overflow is checked during multiplication
--and if thrown will throw over error here
--will runtime error if there is overflow
checkMulOverflow :: [Instruction]
checkMulOverflow
  = [ LABEL "p_throw_overflow_error",
      LDR W AL (FinalGpReg 0) (LabAdd "int_overflow_error"),
      BL AL "p_throw_runtime_error"]

--Adds assembly code to check for divide by zero
--will runtime error if we try to divide by zero.
checkDivideByZero :: [Instruction]
checkDivideByZero
  = [ LABEL "p_check_divide_by_zero",
      PUSH AL (Registers [ReturnPointer]),
      CMP AL (FinalGpReg 1) (ImmInt 0),
      LDR W EQu (FinalGpReg 0) (LabAdd "divide_by_zero_error"),
      BL EQu "p_throw_runtime_error",
      POP AL (Registers [ProgramCounter]) ]

--This throws the runtime error
--Exits the program with error code -1
throwRuntimeError :: [Instruction]
throwRuntimeError 
  = [ LABEL "p_throw_runtime_error",
      BL AL "p_print_string",
      MOV No AL (FinalGpReg 0) (ImmInt (-1)),
      BL AL "exit"]

