{
{- # OPTIONS -w #-}
module Parser where

import Token 
import Lexer
import Grammar

import Control.Applicative
import Control.Monad
}

%name       waccParse
%tokentype  { TokenItem }
%monad      { Alex }
%lexer      { lexwrap } { TokenItem _ TokenEOF }
%error      { happyError }

%token
  begin        { TokenItem _ TokenBegin }
  end          { TokenItem _ TokenEnd }
  is           { TokenItem _ TokenIs }

  skip         { TokenItem _ TokenSkip }
  read         { TokenItem _ TokenRead  }
  free         { TokenItem _ TokenFree }
  return       { TokenItem _ TokenReturn }
  exit         { TokenItem _ TokenExit }
  print        { TokenItem _ TokenPrint }
  println      { TokenItem _ TokenPrintLn }

  import       { TokenItem _ TokenImport }

  struct       { TokenItem _ TokenStruct }
  newstruct    { TokenItem _ TokenNewStruct }
  
  File         { TokenItem _ TokenFile }
  open         { TokenItem _ TokenOpenFile }
  close        { TokenItem _ TokenCloseFile }
  readFile     { TokenItem _ TokenReadFile }
  readLnFile   { TokenItem _ TokenReadLnFile }
  writeFile    { TokenItem _ TokenWriteFile }

  if           { TokenItem _ TokenIf  }
  then         { TokenItem _ TokenThen  }
  else         { TokenItem _ TokenElse  }
  fi           { TokenItem _ TokenFi}

  while        { TokenItem _ TokenWhile }
  do           { TokenItem _ TokenDo   }
  done         { TokenItem _ TokenDone   }

  newpair      { TokenItem _ TokenNewPair}
  call         { TokenItem _ TokenCall  }

  fst          { TokenItem _ TokenFst   }
  snd          { TokenItem _ TokenSnd  }
  pair         { TokenItem _ TokenPair }
  pairlit      { TokenItem _ TokenPairLiter }

  int          { TokenItem _ TokenInt }
  bool         { TokenItem _ TokenBool }
  char         { TokenItem _ TokenChar }
  str          { TokenItem _ TokenString }

  intlit       { TokenItem _ (TokenIntLit $$) }
  boollit      { TokenItem _ (TokenBoolLit $$) }
  charlit      { TokenItem _ (TokenCharLit $$) }
  strlit       { TokenItem _ (TokenStringLit $$) }

  '{'          { TokenItem _ TokenLeftCurlyBracket }
  '}'          { TokenItem _ TokenRightCurlyBracket }
  '('          { TokenItem _ TokenLeftRoundBracket }
  ')'          { TokenItem _ TokenRightRoundBracket }
  '['          { TokenItem _ TokenLeftSquareBracket }
  ']'          { TokenItem _ TokenRightSquareBracket }
  ','          { TokenItem _ TokenComma }
  ';'          { TokenItem _ TokenSemiColon }
  '.'          { TokenItem _ TokenPeriod }

  '!'          { TokenItem _ TokenBang }
  len          { TokenItem _ TokenLen }
  ord          { TokenItem _ TokenOrd }
  chr          { TokenItem _ TokenChr }

  ident        { TokenItem _ (TokenId $$) }

  '*'          { TokenItem _ TokenMult }
  '/'          { TokenItem _ TokenDiv } 
  '%'          { TokenItem _ TokenMod } 
  '+'          { TokenItem _ TokenAdd  }
  '-'          { TokenItem _ TokenSub  }
  '>='         { TokenItem _ TokenGT  }  
  '>'          { TokenItem _ TokenSGT  }
  '<'          { TokenItem _ TokenSLT  } 
  '<='         { TokenItem _ TokenLT  }   
  '=='         { TokenItem _ TokenEQ  }  
  '!='         { TokenItem _ TokenNEQ  }
  '&&'         { TokenItem _ TokenAnd  }
  '||'         { TokenItem _ TokenOr  }  
  '='          { TokenItem _ TokenAssign }
                     
%left '&&'
%left '||'
%left '==' '!='
%nonassoc '>=' '>' '<=' '<'
%left '+' '-'
%left '*' '/' '%'
%right '!'

%%                   
                     
Prog    :: {Program} 
Prog    : Imports Structs begin Functions Statement end { (Program $1 $2 $4 $5 )}
        | Imports Structs begin Statement end    { (Program $1 $2 [] $4 )}
        | Imports begin Functions Statement end { (Program $1 [] $3 $4 )}
        | Structs begin Functions Statement end { (Program [] $1 $3 $4 )}
        | Imports begin Statement end { (Program $1 [] [] $3) }
        | Structs begin Statement end { (Program [] $1 [] $3) }
        | begin Functions Statement end  { (Program [] [] $2 $3 )}
        | begin Statement end{ (Program [] [] [] $2) }
        | Structs Imports begin Statement end   {%^ msgError "parse error: Imports must precede Struct declarations"}
        | begin end      {%^ msgError "parse error: No statement in scope!"}

Imports :: {[Imp]}
Imports : Import          { [$1] }
        | Imports Import  { $2 : $1 }

Import :: {Imp}
Import : import strlit              { Import (getLineNumber $1) $2 }


Structs :: {[Struct]}
Structs : SingleStruct          { [$1] }
        | Structs SingleStruct  { $2 : $1 }

SingleStruct :: { Struct }
SingleStruct : struct ident '{' StructStatement '}' ';'
                { StructDef (getLineNumber $1) (StructType $2) $4 }

StructStatement   :: {StructStat}
StructStatement   : 
        skip                      { StructEmpty}
      | Type Ident                { StructDeclare $1 $2 }
      | newstruct ident Ident     { StructDeclare (StructType $2) $3 }
      | StructStatement ';' StructStatement   { StructStatCompose $1 $3 }

Functions   :: {[Func]}
Functions  : Function      { [$1] }
           |  Functions Function{ $2 : $1 }
                     
                     
Function   :: {Func}  
Function  : Type Ident '(' ParamList ')' is Statement end
                { FuncDef $1 $2 (ParamList $4) $7 }
      | Type Ident '(' ')' is Statement end
                { FuncDef $1 $2 (ParamList []) $6 }
      |  Ident '(' ')' is Statement end   
                { %^ msgError "Function does not declare its return type" }
      | Type '(' ')' is Statement end
                { %^ msgError "Function does not have a name!" }
      | '(' ')' is Statement end
                { %^ msgError "Function has neither a name nor a type" }
      | Type Ident '(' ')' is end
                { %^ msgError "Function has no statement" }
                     
ParamList   :: {[Param]}
ParamList  : Param ',' ParamList{ $1 : $3 }
      | Param        { [$1] }
                     
Param     :: {Param} 
Param    : Type Ident    { ParamDef $1 $2 }
                     
Type     :: {Type}    
Type    : BaseType  { BaseType $1 }
      | ArrayType   { ArrayType $1 }
      | PairType    { PairType $1 }
      | Ident       {%^ msgError ("Invalid type entered: " ++ 
                        (let Id id = $1 in show id)) }
                     
ArrayType  :: {ArrayType}
ArrayType  : Type '[' ']'    { ArrayInstance $1 }
                     
PairType  :: {PairType}
PairType  : pair '(' PairElemType ',' PairElemType ')' { PairInstance $3 $5 }
                     
PairElemType:: {PairElemType}
PairElemType: BaseType { BasicPairElem $1 }
      | ArrayType      { ArrayPairElem $1 }
      | pair           { PairPairElem }
                     
BaseType  :: {BaseType}
BaseType  : int     { IntType }
      | bool        { BoolType }
      | char        { CharType }
      | str         { StringType }
      | File        { FileType }
                     

Ident     :: {Ident} 
Ident    : ident           { Id $1 }
         | Ident '.' Ident { SElemId $1 $3 }
                     
Statement   :: {Stat}
Statement   : skip                { Skip (getLineNumber $1)}
      | Type Ident '=' AssignRHS  { StatDeclare (getLineNumber $3) $1 $2 $4 }
      | newstruct ident Ident     { StatStructDeclare (getLineNumber $1) (StructType $2) $3 }
      | AssignLHS '=' AssignRHS   { StatAssign (getLineNumber $2) $1 $3 }
--      | AssignLHS '=' Statement   {%^ msgError "Incorrect assignment"}
      | read AssignLHS            { StatRead (getLineNumber $1) $2 }
--      | read Statement            {%^ msgError "read: Cannot assign to statement"}
--      | read Expression           {%^ msgError "read: Cannot assign to expression"}
      | free Expression           { StatFree (getLineNumber $1) $2 }
      | free Statement            {%^ msgError "free: Cannot free a statement"}
      | return Expression         { StatReturn (getLineNumber $1) $2 }
      | return Statement          {%^ msgError "return: Cannot return a Statement"}
      | exit Expression           { StatExit (getLineNumber $1) $2 }
      | exit Statement            {%^ msgError "exit: Cannot print a statement"}
      | print Expression          { StatPrint (getLineNumber $1) $2 }
--      | print Statement           {%^ msgError "print: Cannot print a statement"}
      | println Expression        { StatPrintln (getLineNumber $1) $2 }
--      | println Statement         {%^ msgError "println: Cannot print a statement"}
      | if Expression then Statement else Statement fi
                { StatIf (getLineNumber $1) $2 $4 $6 }
      | if Expression then Statement
                {%^ msgError "if: no 'else' clause"}
      | if Expression then Statement else Statement
                {%^ msgError "if: No fi declaration. Probable fix: adding 'fi' after end of else Statement"}
      | while Expression do Statement done
                { StatWhile (getLineNumber $1) $2 $4 }
      | while do Statement done
                {%^ msgError "while: No Expression provided for while loop"}
      | while Expression do done
                {%^ msgError "while: No statement(s) in while loop body"}
      | while do done
                {%^ msgError "while: No statement or expression provided"}
      | while Expression do Statement
                {%^ msgError "while: No 'done' statement at the end of while loop"}
      | begin Statement end
                { StatNewScope (getLineNumber $1) $2 }
      | close '('  Expression ')' 
                { StatCloseFile (getLineNumber $1) $3 }
      | writeFile '(' Expression ',' Expression ')'
                { StatFileWrite (getLineNumber $1) $3 $5 } 
      | Statement ';' Statement   { StatCompose (getLineNumber $2) $1 $3 }

SStatement   :: {Stat}
SStatement   : Statement     {$1}
             | Statement ';' {$1}

AssignLHS  :: {AssignLhs}
AssignLHS  : Ident  { AssToIdent $1 }
      | ArrayElem   { AssToArray $1 }
      | PairElem    { AssToPair $1 }


ArrayElem  :: {ArrayElem}
ArrayElem  : Ident ArrayIndices{ ArrayElem $1 $2 }

ArrayIndices:: {[Expr]}
ArrayIndices: '[' Expression ']' ArrayIndices   { $2 : $4 }
      | '[' Expression ']'                      { [$2] }

PairElem  :: {PairElem}
PairElem  : fst Expression  { PairFst $2 }
      | snd Expression      { PairSnd $2 }


AssignRHS  :: {AssignRhs}
AssignRHS  : Expression             { AssignExpr $1 }
      | ArrayLiter                  { AssignArrayLit $1 }
      | newpair '(' Expression ',' Expression ')'
                { AssignNewPair $3 $5 }
      | PairElem          { AssignPairElem $1 }
      | call Ident '(' ArgList ')'   { AssignCall $2 $4 }
      | call Ident '(' ')'           { AssignCall $2 [] }
      | open '(' FileLocation ')'    { AssignFileOpen $3}
      | readFile '(' Expression ')'  { AssignFileRead $3}
      | readLnFile '(' Expression ')'{ AssignFileReadLn $3}

FileLocation :: {FileLocation}
FileLocation : strlit               { FileLocation $1 } 

ArrayLiter :: {ArrayLit}
ArrayLiter : '['  ']'   { ArrayLit [] }
      | '[' ArgList ']'  { ArrayLit $2 }


ArgList :: {[Expr]}
ArgList : Expression ',' ArgList   { $1 : $3 }
      | Expression                    { [$1] }

Expression :: {Expr}
Expression  : '-' intlit {%^ let x = negate $2 in 
				(\s -> return (IntLiteral x))}
      | intlit  {%^ if (($1) < negate (2^31) || $1 >= 2^31)
                        then (msgError "Integer out of range")
                        else (\s -> return (IntLiteral $1)) }
      | boollit       { BoolLiteral $1 }
      | charlit       { CharLiteral $1 }
      | strlit        { StringLiteral $1 }
      | pairlit       { PairLiteral Null }
      | Ident         { IdentExpr $1 }
      | ArrayElem     { ArrayElemExpr $1 }
      | UnOp Expression  { UnOpExpr $1 $2 }
      | Expression '*'  Expression   { BinOpExpr MultOp $1 $3 }
      | Expression '/'  Expression   { BinOpExpr DivOp  $1 $3 }
      | Expression '%'  Expression   { BinOpExpr ModOp  $1 $3 }
      | Expression '+'  Expression   { BinOpExpr PlusOp $1 $3 }
      | Expression '-'  Expression   { BinOpExpr MinOp  $1 $3 }
      | Expression '>'  Expression   { BinOpExpr GTOp   $1 $3 }
      | Expression '>=' Expression   { BinOpExpr GTEqOp $1 $3 }
      | Expression '<'  Expression   { BinOpExpr LTOp   $1 $3 }
      | Expression '<=' Expression   { BinOpExpr LTEqOp $1 $3 }
      | Expression '==' Expression   { BinOpExpr EqOp   $1 $3 }
      | Expression '!=' Expression   { BinOpExpr NEqOp  $1 $3 }
      | Expression '&&' Expression   { BinOpExpr AndOp  $1 $3 }
      | Expression '||' Expression   { BinOpExpr OrOp   $1 $3 }
      | '(' Expression ')'  { BracketedExpr $2 } 



UnOp :: {UnOp}
UnOp  : '!'     { FactOp }
      | '-'     { NegOp }
      | len     { LenOp }
      | ord     { OrdOp }
      | chr     { ChrOp }


{

-- Wrapper for the lexer for happy
lexwrap :: (TokenItem -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

-- Default Happy Parse Errors if error does not exist
happyError :: TokenItem -> Alex a
happyError (TokenItem p t) =
  alexError' p ("parse error at token '" ++ unToken t ++ "'")

-- Defining Happy Parse Errors for specific errors defined above
msgError :: String -> TokenItem -> Alex a
msgError msg (TokenItem p t) = 
  alexError' p ("parse error immediately before '" ++ unToken t ++ "': " ++ msg ++ "\n")

-- For Passing a Line Number of a valid statment onto the 
-- Semantic Analyser
getLineNumber :: TokenItem -> (Int,Int)
getLineNumber (TokenItem (AlexPn _ l c) _) = (l,c)

-- Entry Point method for the parser
parseExp :: FilePath -> String -> Either String Program
parseExp = runAlex' waccParse

}
