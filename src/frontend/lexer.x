-- lexer.x
--
-- Alex Specification for generating a monadic 
-- lexer for our while language
--

-- Initial Code
{
{-# OPTIONS -w #-}
module Lexer
  ( AlexPosn(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , TokenItem(..)
  ) where

import Data.Maybe 
import Prelude hiding (lex)
import Control.Monad (liftM)
import Token
}




-- Wrapper

%wrapper "monadUserState"


-- Macros

$digit = [0-9]
$octdig = [0-7]
$hexdig = [0-9A-Fa-f]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$invalidChar = [\" \' \\]
$validPrintable = $printable # $invalidChar
$graphic = $printable # $white
$letter = [a-zA-Z]
$nonletter = [~$letter\n]
$identifiers = [$letter$digit\_]
$escape = [0btnfr\"\'\\]

@string = \" ($printable)* \"
@char = \' (($validPrintable) | \\($escape)) \'
@bool = ( "true" | "false" )
@id = ($letter | \_)($identifiers*)
--@char = ($graphic # $special) | @escape
--@escape = ’\\’ ($printable | ’x’ $hexdig+ | ’o’ $octdig+ | $digit+)



-- Rules
  tokens :- 

    -- ignoring...
    $white+                         ;
    "#".*                           ;

    -- program
    begin                           { lex' TokenBegin }
    end                             { lex' TokenEnd }
    is                              { lex' TokenIs }

    -- commands 1
    skip                            { lex' TokenSkip }
    read                            { lex' TokenRead }
    free                            { lex' TokenFree }
    return                          { lex' TokenReturn }
    exit                            { lex' TokenExit }
    print                           { lex' TokenPrint }
    println                         { lex' TokenPrintLn }

    -- imports
    import                          { lex' TokenImport }

    -- structs
    struct                          { lex' TokenStruct }
    newstruct                       { lex' TokenNewStruct }

    -- File IO
    open                            { lex' TokenOpenFile }
    close                           { lex' TokenCloseFile }
    readFile                        { lex' TokenReadFile }
    readLnFile                      { lex' TokenReadLnFile }
    writeFile                       { lex' TokenWriteFile }

    -- if statment
    if                              { lex' TokenIf }
    then                            { lex' TokenThen }
    else                            { lex' TokenElse }
    fi                              { lex' TokenFi }

    -- while loop
    while                           { lex' TokenWhile }
    do                              { lex' TokenDo }
    done                            { lex' TokenDone }

    -- pair creation
    newpair                         { lex' TokenNewPair }
    call                            { lex' TokenCall }

    -- pair access
    fst                             { lex' TokenFst }
    snd                             { lex' TokenSnd }
    pair                            { lex' TokenPair }

    
    -- types
    int                             { lex' TokenInt }
    bool                            { lex' TokenBool }
    char                            { lex' TokenChar }
    string                          { lex' TokenString }
    File                            { lex' TokenFile }

    -- unop
    \!                              { lex' TokenBang }
    len                             { lex' TokenLen }
    ord                             { lex' TokenOrd }
    chr                             { lex' TokenChr }

    -- binop
    \+                              { lex' TokenAdd }
    \-                              { lex' TokenSub }
    \=\=                            { lex' TokenEQ }
    \=                              { lex' TokenAssign }
    "!="                            { lex' TokenNEQ }
    \*                              { lex' TokenMult }
    \%                              { lex' TokenMod }
    \/                              { lex' TokenDiv } 
    \>                              { lex' TokenSGT }
    ">="                            { lex' TokenGT }
    \<                              { lex' TokenSLT }
    "<="                            { lex' TokenLT }
    "&&"                            { lex' TokenAnd }
    "||"                            { lex' TokenOr }
    
    -- symbols
    \(                              { lex' TokenLeftRoundBracket }
    \)                              { lex' TokenRightRoundBracket }
    \{                              { lex' TokenLeftCurlyBracket }
    \}                              { lex' TokenRightCurlyBracket }
    \[                              { lex' TokenLeftSquareBracket }
    \]                              { lex' TokenRightSquareBracket }
    \'                              { lex' TokenSingleQuote }
    \"                              { lex' TokenDoubleQuote }
    \,                              { lex' TokenComma }
    \;                              { lex' TokenSemiColon }
    \:                              { lex' TokenColon }
    \.                              { lex' TokenPeriod }

    
    -- literals
    $digit+                         { lex (TokenIntLit . read) }
    @char                           { lex (TokenCharLit . lexChar) }
    @string                         { lex (TokenStringLit . lexString) }
    @bool                           { lex (TokenBoolLit . (flip (==) "true")) }
    null                            { lex' TokenPairLiter }
    
    -- identifier 
    @id                             { lex TokenId }


    $printable                      { lex (TokenUnidentified . head) }

-- Main
{

-- The List of recognised escaped characters
escapes =  [("\\\'","\'"),("\\\"","\""),("\\n","\n"),("\\b","\b"),
              ("\\0","\0"),("\\t","\t"),("\\f","\f"),("\\r","\r"),("\\\\","\\")]

lexChar :: String -> Char
lexChar "'\\\"'" = '\"'
lexChar "'\\\\'" = '\\'
lexChar "'\\\''" = '\''
lexChar "'\\0'" = '\0'
lexChar "'\\n'" = '\n'
lexChar "'\\t'" = '\t'
lexChar "'\\f'" = '\f'
lexChar "'\\r'" = '\r'
lexChar "'\\b'" = '\b'
lexChar s = s !! 1
	

-- The Function which fixes the String input
lexString :: String -> String
lexString s = tail (take ((length s) - 1) s)

-- Function which normalises all escapes in a string
replaceEscaped :: String -> String
replaceEscaped []      = []
replaceEscaped [a]     = [a]
replaceEscaped (a:b:cs)
            | elem [a,b] escapeList = (fromJust (lookup ([a,b]) escapes)) ++ replaceEscaped cs
            | otherwise             = a:replaceEscaped(b:cs)
          where 
              escapeList = map fst escapes

-- The token type, consisting of the source code position and a token class.
data TokenItem = TokenItem AlexPosn Token
  deriving ( Show )

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- Set Alex End Of File
alexEOF :: Alex TokenItem
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ TokenItem p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> Token) -> AlexAction TokenItem
lex f = \(p,_,_,s) i -> return $ TokenItem p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: Token -> AlexAction TokenItem
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex TokenItem
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

}
