module Asm where


--Program Type
data AsmProgram = AsmProgram [Instruction]

instance Show AsmProgram where
        show (AsmProgram [])     = ""
        show (AsmProgram (l:ls)) = show l ++ "\n" ++ show (AsmProgram ls)

--Base instruction type
                  --Data movement instructions
data Instruction = MOV  UpdateAPSR Condition Register Operand2      
                   --Move Op2 into Register
                 | MVN  UpdateAPSR Condition Register Operand2      
                   --MOV not
                 | LDM  Condition AddressMode Register Registers    
                   --Load from memory
                 | STM  Condition AddressMode Register Registers    
                   --Store in memory
                 | PUSH Condition Registers                         
                   --Push (equiv. to STMDB r13)
                 | POP  Condition Registers                         
                   --Pop  (equiv. to LDMIA r13)
                 | LDR  MemType Condition Register Operand2Mem
                   --Load Register from the memory location given by the offset
                 | STR  MemType Condition Register Operand2Mem
                   --Store Register into the memory location given by the offset

                 --Arithemetic operations
                 | ADD  UpdateAPSR Condition Register Register Operand2 --Add Rn to Op2, place result into Rd
                 | MUL  Condition Register Register Register Register --multiply Rn by Op2, place 32 least significant bits into Rd
-- THIS DOES NOT EXIST IN ARMV6  | SDIV UpdateAPSR Condition Register Register Operand2 --divide Rn by Op2, place result into Rd (signed integer division)
                 | ADDW UpdateAPSR Condition Register Register Int --Same as ADD, but using an immediate 12-bit value instead of Op2
                 | SUB  UpdateAPSR Condition Register Register Operand2 --Subtract Op2 from Rn, place result into Rd
                 | RSB  UpdateAPSR Condition Register Register Operand2 --Subtract Rn from Op2, place result into Rd
                 | SUBW UpdateAPSR Condition Register Register Int --Same as SUB, but using an immediate 12-bit value instead of Op2

                 --Logical operations
                 | AND  UpdateAPSR Condition Register Register Operand2 --Bitwise logical AND between Rn and Op2, places the result in Rd
                 | ORR  UpdateAPSR Condition Register Register Operand2 --Bitwise logical OR (bit set) between Rn and Op2, places the result in Rd
                 | EOR  UpdateAPSR Condition Register Register Operand2 --Bitwise logical EXCLUSIVE OR between Rn and Op2, places the result in Rd
                 | BIC  UpdateAPSR Condition Register Register Operand2 --Bitwise logical AND NOT (bit clear) between Rn and Op2, places the result in Rd
                 | ORN  UpdateAPSR Condition Register Register Operand2 --Bitwise logical OR NOT between Rn and Op2, places the result in Rd
                  
                 --Data Processing instructions
                 | CMP Condition Register Operand2 --Compares by doing Rn - Op2 and sets z flag if register values are equal
                 | CMN Condition Register Operand2 --Compare negative by doing Rn + Op2 and sets z flag if register values are equal
                 | TST Condition Register Operand2 --Test bitwise by Rn && Op2 and sets z flag if register values are equal
                 | TEQ Condition Register Operand2 --Test bitwise equality by Rn EOR Op2 and sets z flag if register values are equal

                 --Branch instructions
                 | B    Condition String --Branch to position 
                 | BL   Condition String --Branch to position Op2 after storing current program counter in link register

                 --Insert Label
                 | LABEL String --Inserts a label at the current position
                 --Insert section header
                 | Header String
                 --data declare
                 | DataDeclare Declare

                 --Temporary Instructions
                 | StoreString Register String
                   --Stores a string into the register r
                 | PrintString
                 | PrintChar
                 | PrintInt
                 | PrintBool
                 | PrintPointer
                 | PrintLn

                 | ReadInt
                 | ReadChar
                 
                 | Free
                 | FreeStruct
                 | OutOfBoundsRuntimeError
                 | IntegerOverflowUnderflowRuntimeError
                 | MulOverflowUnderflowRuntimeError
                 | DivideByZeroRuntimError
                 | NullPointerRuntimeError
                 deriving Eq

instance Show Instruction where
        show (MOV  s cond rd op2) = "\tMOV"  ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ show op2
        show (MVN  s cond rd op2) = "\tMVN"  ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ show op2
        show (LDM  cond mode rd rs) = "\tLDM"  ++ show cond ++ show mode ++ " " ++ show rd ++ ", " ++ show rs
        show (STM  cond mode rd rs) = "\tSTM"  ++ show cond ++ show mode ++ " " ++ show rd ++ ", " ++ show rs
        show (LDR  mt cond rd ro) = "\tLDR"  ++ show mt ++ show cond ++ " " ++ show rd ++ ", "  ++ show ro
        show (STR  mt cond rd ro) = "\tSTR"  ++ show mt ++ show cond ++ " " ++ show rd ++ ", "  ++ show ro
        show (PUSH cond rs)     = "\tPUSH" ++ show cond ++ " " ++ show rs
        show (POP  cond rs)     = "\tPOP"  ++ show cond ++ " " ++ show rs

        show (ADD s cond rd rn op2) = "\tADD"   ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ (if rn /= rd then show rn ++ ", " else "") ++ show op2
        show (ADDW s cond rd rn i)  = "\tADDW"  ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ (if rn /= rd then show rn ++ ", " else "") ++ show i
        show (SUB s cond rd rn op2) = "\tSUB"   ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ (if rn /= rd then show rn ++ ", " else "") ++ show op2
        show (SUBW s cond rd rn i)  = "\tSUBW"  ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ (if rn /= rd then show rn ++ ", " else "") ++ show i
        show (RSB s cond rd rn op2) = "\tRSB"   ++ show s ++ show cond ++ " " ++ show rd ++ ", " ++ (if rn /= rd then show rn ++ ", " else "") ++ show op2
        show (MUL cond rdl rdh rn rm) = "\tSMULL" ++ show cond ++ " " ++ show rdl ++ ", " ++ show rdh ++ ", " ++ show rn ++ ", " ++ show rm

        show (AND s cond rd rn op2) = "\tAND" ++ show s ++ show cond ++ " " ++ show rd ++ (if rn /= rd then ", " ++ show rn else "") ++ ", " ++ show op2
        show (ORR s cond rd rn op2) = "\tORR" ++ show s ++ show cond ++ " " ++ show rd ++ (if rn /= rd then ", " ++ show rn else "") ++ ", " ++ show op2
        show (EOR s cond rd rn op2) = "\tEOR" ++ show s ++ show cond ++ " " ++ show rd ++ (if rn /= rd then ", " ++ show rn else "") ++ ", " ++ show op2
        show (BIC s cond rd rn op2) = "\tBIC" ++ show s ++ show cond ++ " " ++ show rd ++ (if rn /= rd then ", " ++ show rn else "") ++ ", " ++ show op2 
        show (ORN s cond rd rn op2) = "\tORN" ++ show s ++ show cond ++ " " ++ show rd ++ (if rn /= rd then ", " ++ show rn else "") ++ ", " ++ show op2

        show (CMP cond rn op2) = "\tCMP" ++ show cond ++ " " ++ show rn ++ ", " ++ show op2
        show (CMN cond rn op2) = "\tCMN" ++ show cond ++ " " ++ show rn ++ ", " ++ show op2
        show (TST cond rn op2) = "\tTST" ++ show cond ++ " " ++ show rn ++ ", " ++ show op2
        show (TEQ cond rn op2) = "\tTEQ" ++ show cond ++ " " ++ show rn ++ ", " ++ show op2

        show (B  cond lab) = "\tB" ++ show cond ++ " " ++ lab 
        show (BL cond lab) = "\tBL"++ show cond ++ " " ++ lab

        show (LABEL lab) = lab ++ ":"
        show (Header s) = s
        show (DataDeclare d) = "\t" ++ show d

        show (StoreString rd str) = "\t; StoreString"  ++ " " ++ show rd ++ ", " ++ show str
        show PrintString      = "\t; PrintString"
        show PrintChar        = "\t; PrintChar"    
        show PrintInt         = "\t; PrintInt"      
        show PrintBool        = "\t; PrintBool"      
        show PrintPointer     = "\t; PrintPointer"  
                                                  
        show PrintLn          = "\t; PrintLn"

        show ReadInt          = "\t; ReadInt"
        show ReadChar         = "\t; ReadChar"
        show Free             = "\t; Free"
        show FreeStruct       = "\t; FreeStruct"

        show OutOfBoundsRuntimeError 
                              = "\t; OutOfBoundsRuntimeError"
        show IntegerOverflowUnderflowRuntimeError
                              = "\t; IntegerOverflowUnderflowRuntimeError"
        show DivideByZeroRuntimError 
                              = "\t; DivideByZeroRuntimError"
                                  
--declare data
data Declare = WORD Int
             | WORDSTRING String
             | ASCII String
             | ASCIIZ String
             deriving Eq

instance Show Declare where
  show (WORD n)   = ".word " ++ show n
  show (WORDSTRING s) = ".word " ++ s
  show (ASCII s)  = ".ascii " ++ "\"" ++ s ++ "\\0" ++ "\""
  show (ASCIIZ s) = ".asciiz " ++ "\"" ++ s ++ "\\0" ++ "\""

--Flag to update the APSR on a particular instruction
data UpdateAPSR = Yes | No
                 deriving Eq

instance Show UpdateAPSR where
        show Yes = "S"
        show No  = ""

--Conditional Execution
data Condition = EQu --If equal
               | NEq --If not equal

               | CS --If carry bit is set
               | CC --If carry bit is not set

               | MI --If negative
               | PL --If positive

               | VS --If overflowed
               | VC --If no overflow

               | HI --Unsigned higher
               | LS --Unsigned lower

               | GE --Signed greater than or equal
               | LE --Signed less than of equal
               | GThan --Signed strictly greater than
               | LThan --Signed strictly less than

               | AL --Always execute
                 deriving Eq

instance Show Condition where
        show EQu   = "EQ"
        show NEq   = "NE"
        show CS    = "CS"
        show CC    = "CC"
        show MI    = "MI"
        show PL    = "PL"
        show VS    = "VS"
        show VC    = "VC"
        show HI    = "HI"
        show LS    = "LS"
        show GE    = "GE"
        show LE    = "LE"
        show GThan = "GT"
        show LThan = "LT"
        show AL    = ""

--Registers, both intermediate and final
data Register = TempGpReg Int
              | FinalGpReg Int
              | FuncArgReg Int
              | StackPointer
              | ReturnPointer
              | ProgramCounter
              deriving Eq

instance Show Register where
        show (TempGpReg n)  = "t" ++ show n
        show (FinalGpReg n) = "r" ++ show n
        show (FuncArgReg n) = "f" ++ show n
        show StackPointer   = "r13"
        show ReturnPointer  = "r14"
        show ProgramCounter = "r15"

data Registers = Registers [Register]
               | RegisterRange Register Register
                 deriving Eq

instance Show Registers where
        show (Registers (r:rs))
          = "{" ++ concat (show r : ["," ++ show x | x <-rs]) ++ "}"
        show (Registers [])
          = ""
        show (RegisterRange r1 r2)
          = "{" ++ show r1 ++ "-" ++ show r2 ++ "}"

--Representation of addressing types for load and store
data AddressMode = IA   --Increment after
                 | IB   --Increment before
                 | DA   --Decrement after
                 | DB   --Decrement before
                 deriving Eq

instance Show AddressMode where
        show IA = "IA"
        show IB = "IB"
        show DA = "DA"
        show DB = "DB"

--Representation of the second operand
data Operand2 = ImmInt Int
              | ImmChar Char
              | ShiftedRegister Register Shift
                 deriving Eq

instance Show Operand2 where
        show (ImmInt n) = "#" ++ show n
        show (ImmChar '\'') = "#'\\\''"
        show (ImmChar '\0') = "#'\\0'"
        show (ImmChar c) = '#' : show c
        show (ShiftedRegister r s) = show r ++ show s

-- Representation of access to an offset of a register
data Operand2Mem = OffsetReg Register  Offset
                 | LabAdd String
                 | IntAdd Int
                 deriving Eq

instance Show Operand2Mem where
        show (OffsetReg rn o) = "[" ++ show rn  ++ show o ++ "]"
        show (LabAdd s) = '=' : s 
        show (IntAdd n) = '=' : show n

data Offset = IntOffset Int
            | RegOffset Register Shift
                 deriving Eq
            -- where shift is between 0 and 3

instance Show Offset where
        show (IntOffset 0 ) = ""
        show (IntOffset n) = ", #" ++ show n
        show (RegOffset rm s) = ", "  ++ show rm ++ show s
          where 
            ss = show s
            d = if ss == "" then "" else ", "
                                      

-- Representation of Load or Store Type
data MemType = UB  -- unsigned byte
             | SB -- signed byte
             | UH  -- unsigned halfword
             | SH -- signed halfword
             | W  -- a Word (should be default)
                 deriving Eq

instance Show MemType where
        show UB = "B"
        show SB = "SB"
        show UH = "H"
        show SH = "SH"
        show W  = ""

--Representation of a barrel shift
data Shift = ASR Int --Arithmetic shift right
           | LSL Int --Logical shift left
           | LSR Int --Logical shift right
           | ROR Int --Rotate right
           | RRX         --Rotate right 1 bit
           | NOS         --No shift, equivalent to "LSL #0"
                 deriving Eq

instance Show Shift where
        show (ASR i)    = ", ASR #" ++ show i
        show (LSL i)    = ", LSL #" ++ show i
        show (LSR i)    = ", LSR #" ++ show i
        show (ROR i)    = ", ROR #" ++ show i
        show RRX        = ", RRX"
        show NOS        = ""
