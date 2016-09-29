module Token
  ( Token(..),
    unToken
  ) where


data Token
      = TokenBegin
      | TokenEnd
      | TokenIs

      | TokenSkip
      | TokenAssign
      | TokenRead
      | TokenFree
      | TokenReturn
      | TokenExit
      | TokenPrint
      | TokenPrintLn

      {-
       -EXTENSION TOKENS
       -}
      
      --Imports
      | TokenImport

      --Structs
      | TokenStruct
      | TokenNewStruct

      --File IO
      | TokenFile
      | TokenOpenFile
      | TokenCloseFile
      | TokenReadFile
      | TokenReadLnFile
      | TokenWriteFile

      {-
       -EXTENSION TOKENS
       -}

      | TokenIf 
      | TokenThen
      | TokenElse
      | TokenFi

      | TokenWhile
      | TokenDo
      | TokenDone

      | TokenNewPair
      | TokenCall

      | TokenFst
      | TokenSnd
      | TokenPair
      | TokenPairLiter

      | TokenInt
      | TokenBool
      | TokenChar 
      | TokenString 

      | TokenIntLit Int
      | TokenBoolLit Bool
      | TokenCharLit Char
      | TokenStringLit String

      | TokenId String

      | TokenLeftRoundBracket
      | TokenRightRoundBracket
      | TokenLeftCurlyBracket
      | TokenRightCurlyBracket
      | TokenLeftSquareBracket
      | TokenRightSquareBracket
      | TokenComma
      | TokenSemiColon
      | TokenSingleQuote
      | TokenDoubleQuote
      | TokenColon
      | TokenPeriod
      
      | TokenEscapeChar

      | TokenBang
      | TokenLen
      | TokenOrd
      | TokenChr

      | TokenMult
      | TokenDiv
      | TokenMod
      | TokenAdd
      | TokenEquals
      | TokenSub
      | TokenSGT
      | TokenGT
      | TokenSLT
      | TokenLT
      | TokenEQ
      | TokenNEQ
      | TokenAnd
      | TokenOr

      | TokenUnidentified Char
      | TokenEOF

      deriving(Show, Eq)


unToken :: Token -> String
unToken TokenBegin = "begin"
unToken TokenEnd = "end"
unToken TokenIs = "is"

unToken TokenSkip = "skip"
unToken TokenAssign = "="
unToken TokenRead = "read"
unToken TokenFree = "free"
unToken TokenReturn = "return"
unToken TokenExit = "exit"
unToken TokenPrint = "print"
unToken TokenPrintLn = "println"

      {-
       -EXTENSION TOKENS
       -}
      
      --Imports
unToken TokenImport = "import"
      --Structs
unToken TokenStruct = "struct"
unToken TokenNewStruct = "newstruct"
      --File IO
unToken TokenFile = "File"
unToken TokenOpenFile = "open"
unToken TokenCloseFile = "close"
unToken TokenReadFile = "readFile"
unToken TokenReadLnFile = "readLnFile"
unToken TokenWriteFile = "writeFile"
      {-
       -EXTENSION TOKENS
       -}
unToken TokenIf = "if"
unToken TokenThen = "then"
unToken TokenElse = "else"
unToken TokenFi = "fi"

unToken TokenWhile = "while"
unToken TokenDo = "do"
unToken TokenDone = "done"

unToken TokenNewPair = "newpair"
unToken TokenCall = "call"

unToken TokenFst = "fst"
unToken TokenSnd = "snd"
unToken TokenPair = "pair"
unToken TokenPairLiter = "null"

unToken TokenInt = "int"
unToken TokenBool = "bool"
unToken TokenChar = "char"
unToken TokenString = "string"

unToken (TokenIntLit i) = show i
unToken (TokenBoolLit b) = show b
unToken (TokenCharLit c) = show c
unToken (TokenStringLit s) = s
unToken (TokenId s) = s

unToken TokenLeftRoundBracket = "("
unToken TokenRightRoundBracket = ")"
unToken TokenLeftCurlyBracket = "{"
unToken TokenRightCurlyBracket = "}"
unToken TokenLeftSquareBracket = "["
unToken TokenRightSquareBracket = "]"
unToken TokenComma = ","
unToken TokenSemiColon = ";"
unToken TokenSingleQuote = "'"
unToken TokenDoubleQuote = "\""
unToken TokenColon = ":"
unToken TokenPeriod = "."

unToken TokenEscapeChar = "escape"

unToken TokenBang = "!"
unToken TokenLen = "len"
unToken TokenOrd = "ord"
unToken TokenChr = "chr"

unToken TokenMult = "*"
unToken TokenDiv = "/"
unToken TokenMod = "%"
unToken TokenAdd = "+"
unToken TokenSub = "-"
unToken TokenSGT = ">="
unToken TokenGT = ">"
unToken TokenSLT = "<="
unToken TokenLT = "<"
unToken TokenEQ = "="
unToken TokenNEQ = "!="
unToken TokenAnd = "&&"
unToken TokenOr = "||"

unToken (TokenUnidentified c) = show c
unToken TokenEOF = "<EOF>"

