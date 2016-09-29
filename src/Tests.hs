{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import Grammar
import Token
import Lexer
import Parser
--import Analyser

main = htfMain htf_thisModulesTests

{- Test tests
test_shouldFail = assertEqual 1 0
test_shouldPass = assertEqual 2 2
-}

-----------------------------------
---------- Test Programs ----------
-----------------------------------

-- The empty program
emptyProgString = "begin\n\
                   \  skip\n\
                   \end"
emptyProgTokens = [TokenBegin, TokenSkip, TokenEnd]
emptyProgParsed = Left $ Program [] Skip
emptyProgAnalysed = Left Nothing

-- The program containing the empty function
simpleFuncString = "begin\n\
                    \  int TestFunc() is\n\
                    \    skip\n\
                    \  end\n\
                    \  skip\n\
                    \end"
simpleFuncTokens = [TokenBegin,TokenInt,TokenId "TestFunc",
                    TokenLeftRoundBracket,TokenRightRoundBracket,
                    TokenIs,TokenSkip,TokenEnd,TokenSkip,TokenEnd]
simpleFuncParsed = Left $ Program [FuncDef (BaseType IntType) (Id "TestFunc") 
                            (ParamList []) Skip] Skip
simpleFuncAnalysed = Left Nothing

-- The program containing an exit statement
simpleExitString = "begin\n\
                    \  exit -1\n\
                    \end"
simpleExitTokens = [TokenBegin, TokenExit, TokenSub, TokenIntLit 1, TokenEnd]
simpleExitParsed = Left $ Program [] (StatExit (UnOpExpr NegOp (IntLiteral 1)))
simpleExitAnalysed = Left Nothing


--Empty program with in-line comment
emptyProgWithCommentString = "begin\n\
                              \  skip # I can write comments in-line\n\
                              \end"
emptyProgWithCommentTokens = [TokenBegin, TokenSkip, TokenEnd]
emptyProgWithCommentParsed = Left $ Program [] Skip
emptyProgWithCommentAnalysed = Left Nothing

--Program containing "println", "&&"
printAndAndProgramString = "begin\n\
                                \  bool a = true ;\n\
                                \  bool b = false ;\n\
                                \  println a && b \n\
                                \end"
printAndAndProgramTokens = [TokenBegin, TokenBool, TokenId "a",
                                TokenAssign, TokenBoolLit True, TokenSemiColon,
                                TokenBool, TokenId "b", TokenAssign,
                                TokenBoolLit False, TokenSemiColon,
                                TokenPrintLn, TokenId "a", TokenAnd,
                                TokenId "b", TokenEnd]
printAndAndProgramParsed = Left $ 
    Program [] (StatCompose 
                   (StatDeclare (BaseType BoolType)
                                (Id "a")
                                (AssignExpr 
                                  (BoolLiteral True))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType BoolType)
                                    (Id "b")
                                    (AssignExpr
                                      (BoolLiteral False))
                       )
                       (StatPrintln (BinOpExpr
                                      (AndOp)
                                      (IdentExpr
                                        (Id "a"))
                                      (IdentExpr
                                        (Id "b"))
                                    )
                       )
                   )
               )
printAndAndProgramAnalysed = Left Nothing


--Program containing "+" operator
addProgramString = "begin\n\
                    \  int x = 5 ;\n\
                    \  int y = 3 ;\n\
                    \  int a = x + y\n\
                    \end"
addProgramTokens = [TokenBegin, TokenInt, TokenId "x", TokenAssign,
                    TokenIntLit 5, TokenSemiColon, TokenInt, TokenId "y",
                    TokenAssign, TokenIntLit 3, TokenSemiColon, TokenInt,
                    TokenId "a", TokenAssign, TokenId "x", TokenAdd,
                    TokenId "y", TokenEnd]
addProgramParsed = Left $ 
    Program [] (StatCompose 
                   (StatDeclare (BaseType IntType)
                                (Id "x")
                                (AssignExpr 
                                  (IntLiteral 5))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType IntType)
                                    (Id "y")
                                    (AssignExpr
                                      (IntLiteral 3))
                       )
                       (StatDeclare (BaseType IntType)
                                    (Id "a")
                                    (AssignExpr 
                                      (BinOpExpr
                                         (PlusOp)
                                         (IdentExpr
                                           (Id "x"))
                                         (IdentExpr
                                           (Id "y"))
                                      )
                                    )
                       )
                   )
               )
addProgramAnalysed = Left Nothing

--Program containing "-" operator
minProgramString = "begin\n\
                    \  int x = 5 ;\n\
                    \  int y = 3 ;\n\
                    \  int a = x - y \n\
                    \end"
minProgramTokens = [TokenBegin, TokenInt, TokenId "x", TokenAssign,
                    TokenIntLit 5, TokenSemiColon, TokenInt, TokenId "y",
                    TokenAssign, TokenIntLit 3, TokenSemiColon, TokenInt,
                    TokenId "a", TokenAssign, TokenId "x", TokenSub,
                    TokenId "y", TokenEnd]
minProgramParsed = Left $ 
    Program [] (StatCompose 
                   (StatDeclare (BaseType IntType)
                                (Id "x")
                                (AssignExpr 
                                  (IntLiteral 5))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType IntType)
                                    (Id "y")
                                    (AssignExpr
                                      (IntLiteral 3))
                       )
                       (StatDeclare (BaseType IntType)
                                    (Id "a")
                                    (AssignExpr 
                                      (BinOpExpr
                                         (MinOp)
                                         (IdentExpr
                                           (Id "x"))
                                         (IdentExpr
                                           (Id "y"))
                                      )
                                    )
                       )
                   )
               )
minProgramAnalysed = Left Nothing

--Program containing "/" operator
divProgramString = "begin\n\
                    \  int x = 5 ;\n\
                    \  int y = 3 ;\n\
                    \  int a = x / y \n\
                    \end"
divProgramTokens = [TokenBegin, TokenInt, TokenId "x", TokenAssign,
                    TokenIntLit 5, TokenSemiColon, TokenInt, TokenId "y",
                    TokenAssign, TokenIntLit 3, TokenSemiColon, TokenInt,
                    TokenId "a", TokenAssign, TokenId "x", TokenDiv,
                    TokenId "y", TokenEnd]
divProgramParsed = Left $ 
    Program [] (StatCompose 
                   (StatDeclare (BaseType IntType)
                                (Id "x")
                                (AssignExpr 
                                  (IntLiteral 5))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType IntType)
                                    (Id "y")
                                    (AssignExpr
                                      (IntLiteral 3))
                       )
                       (StatDeclare (BaseType IntType)
                                    (Id "a")
                                    (AssignExpr 
                                      (BinOpExpr
                                         (DivOp)
                                         (IdentExpr
                                           (Id "x"))
                                         (IdentExpr
                                           (Id "y"))
                                      )
                                    )
                       )
                   )
               )
divProgramAnalysed = Left Nothing

--Program containing "*" operator
multProgramString = "begin\n\
                    \  int x = 5 ;\n\
                    \  int y = 3 ;\n\
                    \  int a = x * y \n\
                    \end"
multProgramTokens = [TokenBegin, TokenInt, TokenId "x", TokenAssign,
                    TokenIntLit 5, TokenSemiColon, TokenInt, TokenId "y",
                    TokenAssign, TokenIntLit 3, TokenSemiColon, TokenInt,
                    TokenId "a", TokenAssign, TokenId "x", TokenMult,
                    TokenId "y", TokenEnd]
multProgramParsed = Left $ 
    Program [] (StatCompose 
                   (StatDeclare (BaseType IntType)
                                (Id "x")
                                (AssignExpr 
                                  (IntLiteral 5))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType IntType)
                                    (Id "y")
                                    (AssignExpr
                                      (IntLiteral 3))
                       )
                       (StatDeclare (BaseType IntType)
                                    (Id "a")
                                    (AssignExpr 
                                      (BinOpExpr
                                         (MultOp)
                                         (IdentExpr
                                           (Id "x"))
                                         (IdentExpr
                                           (Id "y"))
                                      )
                                    )
                       )
                   )
               )
multProgramAnalysed = Left Nothing

--Program containing "%" operator
modProgramString = "begin\n\
                    \  int x = 5 ;\n\
                    \  int y = 3 ;\n\
                    \  int a = x % y \n\
                    \end"
modProgramTokens = [TokenBegin, TokenInt, TokenId "x", TokenAssign,
                    TokenIntLit 5, TokenSemiColon, TokenInt, TokenId "y",
                    TokenAssign, TokenIntLit 3, TokenSemiColon, TokenInt,
                    TokenId "a", TokenAssign, TokenId "x", TokenMod,
                    TokenId "y", TokenEnd]
modProgramParsed = Left $
    Program [] (StatCompose 
                   (StatDeclare (BaseType IntType)
                                (Id "x")
                                (AssignExpr 
                                  (IntLiteral 5))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType IntType)
                                    (Id "y")
                                    (AssignExpr
                                      (IntLiteral 3))
                       )
                       (StatDeclare (BaseType IntType)
                                    (Id "a")
                                    (AssignExpr 
                                      (BinOpExpr
                                         (ModOp)
                                         (IdentExpr
                                           (Id "x"))
                                         (IdentExpr
                                           (Id "y"))
                                      )
                                    )
                       )
                   )
               )
modProgramAnalysed = Left Nothing

{-
--Program containing "<" comparator
ltComparatorProgramString = "begin\n\
                           \  int x = 4 ;\n\
                           \  int y = 2 ;\n\
                           \  bool a = x < y \n\
                           \end"
modProgramTokens = [TokenBegin, TokenInt, TokenId "x", TokenAssign,
                    TokenIntLit 5, TokenSemiColon, TokenInt, TokenId "y",
                    TokenAssign, TokenIntLit 3, TokenSemiColon, TokenInt,
                    TokenId "a", TokenAssign, TokenId "x", TokenMod,
                    TokenId "y", TokenEnd]
modProgramParsed = Left $ 
    Program [] (StatCompose 
                   (StatDeclare (BaseType IntType)
                                (Id "x")
                                (AssignExpr 
                                  (IntLiteral 5))
                   )
                   (StatCompose 
                       (StatDeclare (BaseType IntType)
                                    (Id "y")
                                    (AssignExpr
                                      (IntLiteral 3))
                       )
                       (StatDeclare (BaseType IntType)
                                    (Id "a")
                                    (AssignExpr 
                                      (BinOpExpr
                                         (ModOp)
                                         (IdentExpr
                                           (Id "x"))
                                         (IdentExpr
                                           (Id "y"))
                                      )
                                    )
                       )
                   )
               )
-}

{-
-----------------------------------
-------- Tests for lexer ----------
-----------------------------------

-- Test for the empty program
test_emptyProgLexer = assertEqual emptyProgTokens 
                                  $ alexScanTokens emptyProgString

--Test for the program containing one empty function
test_simpleFuncLexer = assertEqual simpleFuncTokens 
                                  $ alexScanTokens simpleFuncString

--Test for the program containing only an exit statement
test_simpleExitLexer = assertEqual simpleExitTokens 
                                  $ alexScanTokens simpleExitString

--Test for empty program containing inline comment
test_emptyProgWithCommentLexer 
    = assertEqual emptyProgWithCommentTokens
                 $ alexScanTokens emptyProgWithCommentString

--Test for "and" statement and "println" statement
test_printAndBooleanProgramLexer
    = assertEqual printAndAndProgramTokens
                 $ alexScanTokens printAndAndProgramString

--Test for "+" operator
test_addProgramLexer
    = assertEqual addProgramTokens
                 $ alexScanTokens addProgramString

--Test for "-" operator
test_minProgramLexer
    = assertEqual minProgramTokens
                 $ alexScanTokens minProgramString

--Test for "/" operator
test_divProgramLexer
    = assertEqual divProgramTokens
                 $ alexScanTokens divProgramString

--Test for "*" operator
test_multProgramLexer
    = assertEqual multProgramTokens
                 $ alexScanTokens multProgramString

--Test for "%" operator
test_modProgramLexer
    = assertEqual modProgramTokens
                 $ alexScanTokens modProgramString
-}

-----------------------------------
-------- Tests for parser ---------
-----------------------------------

-- Test for the empty program
test_emptyProgParser = assertEqual emptyProgParsed  
                                   $ parseExp emptyProgTokens

--Test for the program containing one empty function
test_simpleFuncParser = assertEqual simpleFuncParsed 
                                    $ parseExp simpleFuncTokens

--Test for the program containing only an exit statement
test_simpleExitParser = assertEqual simpleExitParsed 
                                  $ parseExp simpleExitTokens

--Test for empty program containing inline comment
test_emptyProgWithCommentParser
    = assertEqual emptyProgWithCommentParsed 
                 $ parseExp emptyProgWithCommentTokens

--Test for "and" statement and "println" statement
test_printAndBooleanProgramParser
    = assertEqual printAndAndProgramParsed 
                 $ parseExp printAndAndProgramTokens

--Test for "+" operator
test_addProgramParser
    = assertEqual addProgramParsed 
                 $ parseExp addProgramTokens

--Test for "-" operator
test_minProgramParser
    = assertEqual minProgramParsed 
                 $ parseExp minProgramTokens

--Test for "/" operator
test_divProgramParser
    = assertEqual divProgramParsed 
                 $ parseExp divProgramTokens

--Test for "*" operator
test_multProgramParser
    = assertEqual multProgramParsed 
                 $ parseExp multProgramTokens

--Test for "%" operator
test_modProgramParser
    = assertEqual modProgramParsed 
                 $ parseExp modProgramTokens

-----------------------------------
------- Tests for analyser --------
-----------------------------------

-- Test for the empty program
test_emptyProgAnalyser = assertEqual emptyProgAnalysed  
                                   $ analyse emptyProgParsed

--Test for the program containing one empty function
test_simpleFuncAnalyser = assertEqual simpleFuncAnalysed 
                                    $ analyse simpleFuncParsed

--Test for the program containing only an exit statement
test_simpleExitAnalyser = assertEqual simpleExitAnalysed 
                                  $ analyse simpleExitParsed

--Test for empty program containing inline comment
test_emptyProgWithCommentAnalyser
    = assertEqual emptyProgWithCommentAnalysed 
                 $ analyse emptyProgWithCommentParsed

--Test for "and" statement and "println" statement
test_printAndBooleanProgramAnalyser
    = assertEqual printAndAndProgramAnalysed 
                 $ analyse printAndAndProgramParsed

--Test for "+" operator
test_addProgramAnalyser
    = assertEqual addProgramAnalysed 
                 $ analyse addProgramParsed

--Test for "-" operator
test_minProgramAnalyser
    = assertEqual minProgramAnalysed 
                 $ analyse minProgramParsed

--Test for "/" operator
test_divProgramAnalyser
    = assertEqual divProgramAnalysed 
                 $ analyse divProgramParsed

--Test for "*" operator
test_multProgramAnalyser
    = assertEqual multProgramAnalysed 
                 $ analyse multProgramParsed

--Test for "%" operator
test_modProgramAnalyser
    = assertEqual modProgramAnalysed 
                 $ analyse modProgramParsed


-----------------------------------
-------- Extra functions ----------
-----------------------------------

analyse x = 0
{-
getResult :: ParseResult a -> a
getResult (OK x) = x
getResult (Failed s) = s

instance (Eq a) => Eq (ParseResult a) where
  (OK x) == (OK y) = x == y
  _ == _ = False
-}
