module Grammar where

-- The program
data Program =  Program [Imp] [Struct] [Func] Stat
  deriving (Show, Eq)

-- Imports
data Imp = Import LN String
  deriving (Show, Eq)

-- Structs
data Struct = StructDef LN Type StructStat
  deriving (Show, Eq)

-- Data Type for the contents of structs
data StructStat
          = StructEmpty
          | StructDeclare Type Ident
          | StructStatCompose StructStat StructStat
  deriving (Show, Eq)

-- Functions :
-- Brackets, 'is' and 'end' omitted from type
data Func = FuncDef Type Ident ParamList Stat 
  deriving (Show, Eq)

--List of parameters to pass to a function
data ParamList = ParamList [Param]
  deriving (Show, Eq)

-- Parameters
data Param = ParamDef Type Ident
  deriving (Show, Eq)

-- Statements :
-- '=', 'fi', 'done' and 'end' omitted from type
data Stat = Skip LN
          | StatDeclare LN Type Ident AssignRhs
          | StatStructDeclare LN Type Ident
          | StatAssign LN AssignLhs AssignRhs
          | StatRead LN AssignLhs
          | TypedStatRead LN Type AssignLhs
          | StatFree LN Expr
          | StatReturn LN Expr
          | StatExit LN Expr
          | StatPrint LN Expr
          | StatPrintln LN Expr
          | TypedStatPrint LN Type Expr
          | TypedStatPrintln LN Type Expr
          | StatIf LN Expr Stat Stat
          | StatWhile LN Expr Stat
          | StatBegin LN Stat
          | StatCompose LN Stat Stat
          | StatCloseFile LN Expr
          | StatFileWrite LN Expr Expr 
          | StatNewScope LN Stat
  deriving (Show, Eq)

-- Left-hand side of assignment
data AssignLhs = AssToIdent Ident
               | AssToArray ArrayElem
               | AssToPair PairElem
  deriving (Show, Eq)

-- Right-hand side of assignment :
-- '(', ')' and ',' omitted from type
data AssignRhs = AssignExpr Expr
               | AssignArrayLit ArrayLit
               | AssignNewPair Expr Expr
               | AssignPairElem PairElem
               | AssignCall Ident [Expr]
               | AssignFileOpen FileLocation
               | AssignFileRead Expr
               | AssignFileReadLn Expr
  deriving (Show, Eq)

-- Argument list instead treated as list of Expr i.e. [Expr]

-- Pair element
data PairElem = PairFst Expr
              | PairSnd Expr
  deriving (Show, Eq)

-- Types
data Type = BaseType BaseType
          | ArrayType ArrayType
          | PairType PairType
          | StructType String
          | AnyType
  deriving (Show)
instance Eq Type where
  (==) (BaseType IntCharType) (BaseType IntCharType)
    = True
  (==) _ (BaseType IntCharType)
    = False
  (==) AnyType _
    = True
  (==) _ AnyType
    = True
  (==) (BaseType StringType) (ArrayType (ArrayInstance (BaseType CharType)))
    = True
  (==) (ArrayType (ArrayInstance (BaseType CharType))) (BaseType StringType)
    = True
  (==) (BaseType b1) (BaseType b2)
    = b1 == b2
  (==) (ArrayType a1) (ArrayType a2)
    = a1 == a2
  (==) (PairType p1) (PairType p2)
    = p1 == p2
  (==) (StructType s1) (StructType s2)
    = s1 == s2
  (==) _ _
    = False

-- Basic types
data BaseType = IntType
              | BoolType
              | CharType
              | StringType
              | IntCharType
              | FileType
  deriving (Show, Eq)

-- Array type
data ArrayType = ArrayInstance Type
  deriving (Show)
instance Eq ArrayType where
  (==) (ArrayInstance a1) (ArrayInstance a2)
    = a1 == a2

-- Pair type
data PairType = PairInstance PairElemType PairElemType
              | NullPair
  deriving (Show)
instance Eq PairType where
  (==) (PairInstance pa1 pb1) (PairInstance pa2 pb2)
    = (pa1 == pa2) && (pb1 == pb2)
  (==) NullPair _
    = True
  (==) _ NullPair
    = True

-- Pair element type
data PairElemType = BasicPairElem BaseType
                  | ArrayPairElem ArrayType
                  | PairPairElem
  deriving (Show)
instance Eq PairElemType where
  (==) (BasicPairElem b1) (BasicPairElem b2)
    = b1 == b2
  (==) (ArrayPairElem a1) (ArrayPairElem a2)
    = a1 == a2
  (==) PairPairElem PairPairElem
    = True
  (==) _ _
    = False

-- Expression type :
-- Literals of int, bool, char and string encoded as Expr <Type>
-- '(' <expr> ')' excluded
data Expr = IntLiteral Int
      | BoolLiteral Bool
      | CharLiteral Char
      | StringLiteral String
      | PairLiteral PairLit
      | IdentExpr Ident
      | ArrayElemExpr ArrayElem
      | UnOpExpr UnOp Expr
      | BinOpExpr BinOp Expr Expr
      | BracketedExpr Expr
  deriving (Show, Eq)

-- Unary operator type
data UnOp = FactOp
          | NegOp
          | LenOp
          | OrdOp
          | ChrOp
  deriving (Show, Eq)
         
-- Binary operator type
data BinOp = MultOp
           | DivOp
           | ModOp
           | PlusOp
           | MinOp
           | GTOp
           | GTEqOp
           | LTOp
           | LTEqOp
           | EqOp
           | NEqOp
           | AndOp
           | OrOp
  deriving (Show, Eq)

-- Identifier type 
data Ident = Id String
           | StructBlock Ident [(Ident,Type)]
           | SElemId Ident Ident 
  deriving (Show)

instance Eq Ident where
  (==) (Id str1) (Id str2)
    = str1 == str2
  (==) (StructBlock i1 i) (StructBlock i2 i')
    = i1 == i2 && i == i'
  (==) (StructBlock i1 _ ) i@(Id _)
    = i1 == i
  (==) i@(Id _) (StructBlock i1 _)
    = i1 == i
  (==) (SElemId i1 i2) (SElemId i3 i4)
    = i1 == i3 && i2 == i4
  (==) _ _
    = False

instance Ord Ident where
  (<=) (Id s1) (Id s2)
    = s1 <= s2
  (<=) (SElemId i1 i2) (SElemId i3 i4)
    = if i1 == i3 then i2 <= i4 else i1 <= i3
  (<=) (StructBlock i1 i2) (StructBlock i3 i4)
    =  i1 <= i3
  (<=) (SElemId i1 i2) (StructBlock i3 i4)
    =  i1 <= i3
  (<=) (StructBlock i1 i2) (SElemId i3 i4)
    =  i1 <= i3
  (<=) (SElemId i1 i2) i
    = i1 <= i
  (<=) i (SElemId i1 i2)
    = i <= i1
  (<=) (StructBlock i1 i2) i
    = i1 <= i
  (<=) i (StructBlock i1 i2)
    = i <= i


-- Array element type
data ArrayElem = ArrayElem Ident [Expr]
  deriving (Show, Eq)

-- Array literal type
data ArrayLit = ArrayLit [Expr]
  deriving (Show, Eq)

-- Pair literal type
data PairLit = Null
  deriving (Show, Eq)

-- FilePath type
data FileLocation = FileLocation String
  deriving (Show, Eq)

-- Alias for Line Numbers
type LN = LineNumber
type LineNumber = (Int,Int)

-- Comment data type
data Comment = Comment String
  deriving (Eq, Show)


