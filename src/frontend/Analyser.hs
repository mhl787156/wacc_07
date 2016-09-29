module Analyser where

import Grammar
import Parser
import Data.Maybe
import Data.List
import Control.Arrow

-- name, type
type SymbolTableElem    = (Ident, Type)
type SymbolTable        = [SymbolTableElem]
type FuncTableElem      = (Ident, Type, [Type])
type FuncTable          = [FuncTableElem]


type CheckType  = Either (Maybe Type) String
type FuncReturn = Maybe Type

isRight            :: Either a b -> Bool
isRight (Left a)  = False
isRight _         = True

lookup3       :: Eq a => a -> [(a,b,c)] -> Maybe (a,b,c)
lookup3 _ []  = Nothing
lookup3 a (l@(l1,l2,l3):ls)
  | a == l1   = Just l
  | otherwise = lookup3 a ls

fst3          :: (a,b,c) -> a
fst3 (x,_,_)  = x

combineErrors             :: CheckType -> CheckType -> CheckType
combineErrors (Left a) (Left b)   = Left a
combineErrors (Right a) (Left b)  = Right a
combineErrors (Left a) (Right b)  = Right b
combineErrors (Right a) (Right b) = Right (a ++ "\n\n" ++ b)

combineFuncs             :: (CheckType, [Func]) -> (CheckType, [Func]) -> (CheckType, [Func])
combineFuncs (Left a, ntl) (Left b, ntr)   = (Left a, ntl ++ ntr)
combineFuncs (Right a, ntl) (Left b, ntr)  = (Right a, ntl ++ ntr)
combineFuncs (Left a, ntl) (Right b, ntr)  = (Right b, ntl ++ ntr)
combineFuncs (Right a, ntl) (Right b, ntr) = (Right (a ++ "\n\n" ++ b), ntl ++ ntr)

combineErrorsNoSpacing    :: CheckType -> CheckType -> CheckType
combineErrorsNoSpacing (Left a) (Left b)   = Left a
combineErrorsNoSpacing (Right a) (Left b)  = Right a
combineErrorsNoSpacing (Left a) (Right b)  = Right b
combineErrorsNoSpacing (Right a) (Right b) = Right (a ++ "\n" ++ b)

prependStr                :: String -> CheckType -> CheckType
prependStr str            = combineErrorsNoSpacing (Right str)



--Global Scope, Local Scope
combineSymbolTables       :: SymbolTable -> SymbolTable -> SymbolTable
combineSymbolTables  [] ls    = ls
combineSymbolTables (g'@(g,_):gs) ls  
  | isNothing (lookup g ls)   = g' : combineSymbolTables gs ls
  | otherwise                 = combineSymbolTables gs ls


checkProgram  :: Program -> (CheckType, Program)
checkProgram (Program impl structl fl s)
  = (combinedErrs, programTree) 
    where
        combinedErrs = combineErrors structsCheckType (combineErrors funcsCheckType res)
        (res,_,_,nt) = checkStat stable [] Nothing (getFuncTypes fl) s
        (structsCheckType, stree, stable) = checkStructList structl [] 
        (funcsCheckType, ftree)  = checkFuncList fl
        programTree = Program impl stree ftree nt

--Check Imports List
checkImportList :: [Imp] -> (CheckType, [Imp], SymbolTable)
checkImportList = undefined
  -- checks if file exists
  -- checks if compiled version exists (.s)


--Check Structs List
--in 2 passes 
--first pass checks struct duplication and adds all structs to st,
--second pass adds all of the struct attributes to each struct
checkStructList :: [Struct] -> SymbolTable -> (CheckType, [Struct], SymbolTable)
checkStructList structs st = (ct', structs, st'') 
  where
    (ct, st') = checkStructList' structs st (Left Nothing)
    (ct', st'') = checkStructContents structs st' ct
  -- check for repeated IDs in each struct
  -- check if a struct with that ID is already in symbolTable
  -- Add struct ID to symboltable
checkStructList' :: [Struct] -> SymbolTable -> CheckType -> (CheckType, SymbolTable)
checkStructList' [] st ct = (ct, st)
checkStructList' (StructDef _ stype@(StructType stName) stat :ss) st ct
    = checkStructList' ss updatedSt dupStructErr
    where
      psudoName = Id stName
      inSymbolTable = isJust (lookup psudoName st)
      updatedSt = (StructBlock psudoName [], stype):st --adds struct to symboltable
      dupStructErr=if not inSymbolTable 
                   then Left Nothing 
                   else Right ("Syntax Error: The Struct "
                   ++ show psudoName
                   ++ " was declared twice." 
                   ++ "\nSymbolTable at error: " ++ show updatedSt)

      
checkStructContents :: [Struct] -> SymbolTable -> CheckType -> (CheckType, SymbolTable)
checkStructContents [] st ct = (ct,st)
checkStructContents (StructDef _ stype@(StructType stName) stat: ss) st ct
    = checkStructContents ss st'' ct'
    where 
    (ct', structAttrSt) = checkStructContents' stat ct []
    st'  = (StructBlock (Id stName) structAttrSt, stype):st
    st'' = delete (StructBlock (Id stName) [], stype) st'

    checkStructContents' :: StructStat -> CheckType -> SymbolTable -> (CheckType, SymbolTable)
    checkStructContents' StructEmpty ct st = (ct, st)
    checkStructContents' (StructDeclare t@(StructType name) id) ct st
      = (structContentErr, updatedst)
         where
           updatedst = (id ,t):st
           structContentErr = if isJust (lookup (StructBlock (Id name) []) st)
                              then Left Nothing
                              else Right ( "Syntax Error: The Struct Attribute Id "
                                ++ show id
                                ++ " of type struct does not exist."
                                ++ "\n" ++ show updatedst)

    checkStructContents' (StructDeclare t id) ct st
      = (dupContentErr, updatedst)
         where
           updatedst = (id ,t):st
           dupContentErr = if isNothing (lookup id st)
                           then Left Nothing
                           else Right ( "Syntax Error: The Struct Attribute "
                                ++ show id
                                ++ " was declared twice."
                                ++ "\n" ++ show updatedst)
    checkStructContents' (StructStatCompose ss ss') ct st
      = (combineErrors ct1 ct2, st2)
        where
           (ct1, st1) = checkStructContents' ss ct st
           (ct2, st2) = checkStructContents' ss' ct st1
  

--Check functions list
checkFuncList    :: [Func] -> (CheckType, [Func])
checkFuncList fs = combineFuncs validFuncs (dupFuncsErr, [])
  where
    validFuncs = foldr (combineFuncs . checkFunc funcTable) (Left Nothing, []) fs
    dupFuncsErr= if isNothing myDupFuncs
                 then Left Nothing 
                 else Right ("Syntax Error: The function "
                 ++ show (fromJust myDupFuncs)
                 ++ " was declared twice." 
                 ++ "\n" ++ show funcTable)
    funcTable  = getFuncTypes fs
    myDupFuncs = dupFuncs fs

dupFuncs :: [Func] -> Maybe Ident
dupFuncs fs = dupFuncs' funcTable []
  where
    funcTable = getFuncTypes fs

dupFuncs' :: [FuncTableElem] -> [Ident] -> Maybe Ident
dupFuncs' [] _           = Nothing
dupFuncs' ((id,_,_):fs) is = if id `elem` is 
                             then Just id
                             else dupFuncs' fs (id:is)

getFuncTypes        :: [Func] -> FuncTable
getFuncTypes []      = []
getFuncTypes (f:fs)  = (fId, fType, fParamTypes) : getFuncTypes fs
  where
    FuncDef fType fId (ParamList fParams) _ = f
    fParamTypes = [ tp | (ParamDef tp _) <- fParams]

--Check function
checkFunc      :: FuncTable -> Func -> (CheckType, [Func])
checkFunc ft (FuncDef t i (ParamList pl) st)
  | isRight res     = (res, [])
  | not ret         = (Right ("Semantic error: Function " ++ show i
                             ++ " did not return a value."), [])
  | otherwise       = (Left Nothing, [FuncDef t i (ParamList pl) nt])
    where 
      ls = parseParams [] pl
      (res, _, ret, nt) = checkStat ls [] (Just t) ft st

parseParams  :: SymbolTable -> [Param] -> SymbolTable
parseParams st []                    = st
parseParams st (p@(ParamDef t i):ps) = parseParams ((i,t):st) ps

--Check Statement
--Global Scope, Local Scope, Function Return type, Function Table, Statement
--Returns the result of the check, an Updated Symbol Table, whether the 
--statement is guaranteed to return
checkStat    :: SymbolTable -> SymbolTable -> FuncReturn -> FuncTable -> Stat 
                  -> (CheckType, SymbolTable, Bool, Stat)
checkStat gs ls _ _ st@(Skip ln)
  = (Left Nothing, ls, False, st)

checkStat gs ls _ ft st@(StatDeclare ln t i r)
  | isJust (lookup i ls)  = (Right ("Semantic error on line " ++ show ln
                                        ++ ": Variable " ++ show i
                                        ++ " has already been declared"
                                        ++ " in the current scope."),
                       ls, False, st)
  | isRight rType        = (prependStr ("Semantic error in statement at " 
                                          ++ show ln ++ ": ") rType, ls, False, st)
  | Left (Just t) /= rType  = (Right ("Semantic error on line " ++ show ln 
                  ++ ": Assignment to newly declared variable "
                  ++ show i
                  ++ " is of an incorrect type: could not match "
                  ++ show t
                  ++ " to "
                  ++ (let Left (Just t) = rType in show t)
                            ++ " Global symbol table: " ++ show gs 
                            ++ "Local symbol table: " ++ show ls),
                       ls, False, st)
  | otherwise          = (Left Nothing, (i, t):ls , False, st)
    where
      rType = checkRhs gs ls ft r

checkStat gs ls _ ft st@(StatStructDeclare ln t@(StructType name) i)
  | isJust (lookup i ls)  = (Right ("Semantic error on line " ++ show ln
                                        ++ ": Variable " ++ show i
                                        ++ " has already been declared"
                                        ++ " in the current scope."),
                                     ls, False, st)
--  | isNothing (lookup (Id name) gs)
  | null structBlockswithName
                          = (Right ("Semantic error in statment at "
                              ++ show ln ++ ": Struct Assignment Type does not exist. "
                              ++ show t ++ " has not been defined."
                              ++ "\n GlobalScope: " ++ show gs), ls, False, st)
  | otherwise   = (Left Nothing, (i, t): newLocalScope , False, st)
    where
--    newLocalScope = map (\(ident, t) -> (SElemId (Id name) ident, t)) structAttr ++ ls 
      newLocalScope        = map (first (SElemId i)) structAttr' ++ ls 
      structBlockswithName = [elem | elem@(StructBlock x _,_) <- gs, x == (Id name)]
      structAttr'          = if null structAttr then [] else head structAttr
      structAttr           = [list | (StructBlock x list,_) <- structBlockswithName, x == (Id name)]


checkStat gs ls _ ft st@(StatAssign ln l r)
  | lType /= rType  = (combineErrors mismatchError (combineErrors lType rType),
                                ls, False, st)
  | isRight lType    = (prependStr ("Semantic error in statement at " 
                                         ++ show ln ++ ": ") 
                                     (combineErrors lType rType), ls, False, st)
  | isRight rType    = (prependStr ("Semantic error in statement at " 
                                      ++ show ln ++ ": ") 
                                   (combineErrors lType rType), ls, False, st)
  | otherwise      = (Left Nothing, ls, False, st)
    where
      lType = checkLhs gs ls l
      rType = checkRhs gs ls ft r
      mismatchError = Right ("Semantic error on line " 
              ++ show ln ++ ": Assignment to LHS "
              ++ show l
              ++ " is of an incorrect type: could not match "
              ++ show lType
              ++ " to "
              ++ show rType ++ " Global symbol table: " ++ show gs) 


checkStat gs ls _ _ st@(StatRead ln l)
  | isRight lFail      = (prependStr ("Semantic error in statement at " 
                             ++ show ln ++ ": ") lFail, ls, False, st)
  | otherwise        = case lType of
                         BaseType BoolType -> (Right ("Semantic error on line " 
                                              ++ show ln 
                                              ++ ": read can only assign" 
                                              ++ " to char and int types"),
                                              ls, False, st)
                         PairType _        -> (Right ("Semantic error on line " 
                                              ++ show ln 
                                              ++ ": read can only assign" 
                                              ++ " to char and int types"),
                                              ls, False, st)
                         _                 -> (Left Nothing, ls, False, TypedStatRead ln lType l)
    where
      lFail         = checkLhs gs ls l
      Left lType'   = checkLhs gs ls l
      Just lType    = lType'

checkStat gs ls _ _ st@(StatFree ln exp)
  | isRight exprFail    = (prependStr ("Semantic error in statement at " 
                                        ++ show ln ++ ": ") exprFail, ls, False, st)
  | otherwise        = case eType of
                ArrayType n  -> (Left Nothing, ls, False, st)
                PairType n  -> (Left Nothing, ls, False, st)
                StructType n -> (Left Nothing, ls, False, st)
                _      -> (Right ("Semantic error on line " ++ show ln 
                        ++ ": expression " ++ show exp
                        ++ " could not be freed, as it is not a pair" 
                        ++ " or an array.")
                        , ls, False, st)
    where
      exprFail      = checkExpr gs ls exp
      Left eType'   = exprFail
      Just eType    = eType'

checkStat gs ls fRet _ st@(StatReturn ln exp)
  | isRight exprRet    = (prependStr ("Semantic error in statement at " 
                                ++ show ln ++ ": ") exprRet, ls, True, st)
  | fRet == eType       = (Left Nothing, ls, True, st)
  | isNothing fRet    = (Right ("Semantic error on line " 
                                   ++ show ln ++ ": values cannot be returned"
                                   ++ " from the main function!"), ls, True, st)
  | otherwise        = (Right ("Semantic error on line " ++ show ln 
                ++ ": expression " ++ show exp
                ++ "was of a type not matching that of the function.")
                , ls, True, st)
    where
      exprRet        = checkExpr gs ls exp
      Left eType     = exprRet

checkStat gs ls _ _ st@(StatExit ln exp)
  | isRight exprRet    = (prependStr ("Semantic error in statement at " 
                          ++ show ln ++ ": ") exprRet, ls, True, st)
  | exprRet == Left (Just (BaseType IntType))
                       = (Left Nothing, ls, True, st)
  | otherwise        = (combineErrors exprRet
                       (Right ("Semantic error: the expression"
                       ++ show exp
                       ++ "on line" ++ show ln
                       ++ "does not evaluate to an int,"
                       ++ "and therefore cannot be used in an exit statement"))
                       , ls, True, st)
    where
      exprRet        = checkExpr gs ls exp


checkStat gs ls _ _ (StatPrint ln exp)
  | isRight exprRet    = (prependStr ("Semantic error in statement at " 
                              ++ show ln ++ ": ") exprRet, ls, False, typed)
  | otherwise        = (Left Nothing, ls, False, typed)
    where
      exprRet        = checkExpr gs ls exp
      typed          = let Left (Just exType) = exprRet in TypedStatPrint ln exType exp

checkStat gs ls _ _ (StatPrintln ln exp)
  = (res, ls, False, TypedStatPrintln ln exType exp)
    where
      (pR,_,_,t)    = checkStat gs ls Nothing [] (StatPrint ln exp)
      (TypedStatPrint _ exType _)
                    = t
      res           = if isRight pR
                    then pR
                    else Left Nothing


checkStat gs ls fRet  ft st@(StatIf ln cond iftrue iffalse)
  | isRight lType && isRight rType
              = (prependStr ("Semantic error in statement at " 
                     ++ show ln ++ ": ") (combineErrors lType rType), ls, ret, st)
  | isRight lType      = (prependStr ("Semantic error in statement at " 
                                          ++ show ln ++ ": ") lType, ls, ret, st)
  | isRight rType      = (prependStr ("Semantic error in statement at " 
                                          ++ show ln ++ ": ") rType, ls, ret, st)
  | isRight cType      = (prependStr ("Semantic error in statement at " 
                                          ++ show ln ++ ": ") cType, ls, ret, st)
  | cType == Left (Just (BaseType BoolType))
              = (Left Nothing, ls, ret, StatIf ln cond nt1 nt2)
  | otherwise        = (Right ("Semantic error: the expression "
                  ++ show cond
                  ++ " on line: " ++ show ln 
                  ++ " does not evaluate to a boolean," 
                  ++ " and thus cannot be used as a condition.")
                , ls, ret, st)
    where
      newScope        = combineSymbolTables gs ls
      (lType,_,retl, nt1)  = checkStat newScope [] fRet ft iftrue
      (rType,_,retr, nt2)  = checkStat newScope [] fRet ft iffalse
      cType           = checkExpr gs ls cond
      ret             = retl && retr

checkStat gs ls fRet ft st@(StatWhile ln cond body)
  | isRight bType    = (prependStr ("Semantic error in statement at " 
                                       ++ show ln ++ ": ") bType, ls, False, st)
  | cType == Left (Just (BaseType BoolType))
              = (Left Nothing, ls, False, StatWhile ln cond nt)
  | otherwise        = (Right ("Semantic error: the expression "
                  ++ show cond
                  ++ " on line: " ++ show ln
                  ++ " does not evaluate to a boolean,"
                  ++ " and thus cannot be used as a condition.")
                ,ls, False, st)
    where
      newScope          = combineSymbolTables gs ls
      (bType,_,_, nt)   = checkStat newScope [] fRet ft body
      cType             = checkExpr gs ls cond

checkStat gs ls fRet ft st@(StatCompose ln s1 s2)
  = (combineErrors first second, ls', ret, StatCompose ln nt1 nt2)
    where
      (first, ls', retf, nt1)   = checkStat gs ls fRet ft s1
      (second, _, rets, nt2)    = checkStat gs ls' fRet ft s2
      ret                       = retf || rets

checkStat gs ls fRet ft (StatNewScope ln body)
  = (ctype, ls, ret, StatNewScope ln nt)
    where 
      newScope = combineSymbolTables gs ls
      (ctype, _, ret, nt) = checkStat newScope [] fRet ft body


checkRhs    :: SymbolTable -> SymbolTable -> FuncTable -> AssignRhs -> CheckType
checkRhs gs ls _ (AssignExpr e)
  = checkExpr gs ls e
checkRhs gs ls _ (AssignArrayLit (ArrayLit exprs))
  | null exprs                = Left (Just (ArrayType (ArrayInstance AnyType)))
  | isRight errorsCheckTypes  = errorsCheckTypes
  | nonInts /= []             = Right ("Semantic error: types in array literal "
                                      ++ show exprs
                    ++" did not match.")
  | otherwise                 = Left (Just ( ArrayType (ArrayInstance t))) 
    where
      Left (Just t)  = head checktypes'
      nonInts = filter (\p -> p /= Left (Just  t)) checktypes'
      errorsCheckTypes = foldr1 combineErrors checktypes'
      checktypes' = map (checkExpr gs ls) exprs

checkRhs gs ls _ (AssignNewPair e1 e2)
  | isRight z1 && isRight z2    = combineErrors z1 z2
  | isRight z1                  = z1
  | isRight z2                  = z2 
  | otherwise                   = Left (Just (PairType(PairInstance 
                                      (pairCaseCheck t1) (pairCaseCheck t2))))
    where
      Left (Just t1) = z1
      Left (Just t2) = z2
      z1 = checkExpr gs ls e1
      z2 = checkExpr gs ls e2

      pairCaseCheck :: Type -> PairElemType
      pairCaseCheck (BaseType t )   = BasicPairElem t
      pairCaseCheck (ArrayType t )  = ArrayPairElem t
      pairCaseCheck _                 = PairPairElem

checkRhs gs ls _ (AssignPairElem (PairFst e))
  | isRight z = z
  | otherwise = case x of
          PairType (PairInstance t1 _) -> Left (Just (unPairElemType t1))
          _                            -> Right ("Semantic Error: " ++ show z 
                                           ++ " is Not A Pair. Global scope: " 
                                           ++ show gs ++ "     Local scope: " 
                                           ++ show ls)
    where
      Left (Just x) = z
      z = checkExpr gs ls e 

checkRhs gs ls _ (AssignPairElem (PairSnd e))
  | isRight z = z
  | otherwise =  case x of
          PairType (PairInstance _ t2) -> Left (Just (unPairElemType t2))
          _                            ->  Right ("Semantic Error: " 
                                            ++ show z ++ " is Not A Pair")
    where
      Left (Just x) = z
      z = checkExpr gs ls e 

checkRhs gs ls ft (AssignCall i args)
  | isNothing func            = Right ("Semantic error: the Identifier "
                                          ++ show i ++ 
                                            " does not exist in this scope")
  | null argTypes             = Left (Just typ)
  | isRight argErrs           = argErrs
  | argTypeMismatch           = Right ("Semantic error: the function "
                                      ++ show i ++
                                       " cannot be called with those" 
                                       ++ " argument types.")
  | otherwise                 = Left (Just typ)
    where
      func = lookup3 i ft
      Just(id, typ, funcArgs) = func
      argTypes = map (checkExpr gs ls) args
      argErrs = foldl1 combineErrors argTypes
      argTypeEx = [t | Left (Just t) <- argTypes]
      argTypeMismatch = and (zipWith (/=) argTypeEx funcArgs)
                      || length funcArgs /= length argTypeEx



checkLhs    :: SymbolTable -> SymbolTable -> AssignLhs -> CheckType
checkLhs gs ls (AssToIdent i)
  | isNothing x && isNothing y && not lhsStructExistance
       = Right ("Semantic error: the Identifier " 
                                            ++ show i ++
                                            " does not exist in this scope"
                                            ++ "\nGlobal Scope: " ++ show gs
                                            ++ "\nLocal Scope:  " ++ show ls)
  | isNothing x                 = Left y
  | otherwise                   = Left x
    where 
      x   = lookup i ls
      y   = lookup i gs

      lhsStructExistance = null list1 && null list2 
      list1 = [e | e@(StructBlock x _, _) <- ls , x == i]
      list2 = [e | e@(StructBlock x _, _) <- gs , x == i]

checkLhs gs ls (AssToArray (ArrayElem i exprs))
  | isNothing x && isNothing y  = Right ("Semantic error: the Identifier"
                                           ++ show i ++
                                           " does not exist in this scope")
  | isRight errorsCheckTypes    = errorsCheckTypes
  | zs /= []                    = Right ("Semantic error: the Array Index" 
                                            ++ " is not an Integer")
  | otherwise                   = stripper (length exprs) arrayType
    where
      x = lookup i ls
      y = lookup i gs 
      zs = filter (\p -> p /= Left (Just (BaseType IntType))) checktypes'
      errorsCheckTypes = foldr1 combineErrors checktypes'
      checktypes' = map (checkExpr gs ls) exprs

      Left(Just arrayType) = if isNothing x then Left y else Left x
      
      stripper :: Int -> Type -> CheckType
      stripper 0 t = Left (Just t)
      stripper n (BaseType StringType) = Left (Just (BaseType CharType))
      stripper n (ArrayType(ArrayInstance t)) = stripper (n-1) t
      stripper _ _ = Right "Semantic Error: Dimensional Mismatch"

checkLhs gs ls (AssToPair (PairFst expr))
  | isRight z = z
  | otherwise = case x of
          PairType (PairInstance t1 _) -> Left (Just (unPairElemType t1))
          _                            -> Right ("Semantic Error: " 
                                              ++ show z ++ " is Not A Pair")
    where
      Left (Just x) = z
      z = checkExpr gs ls expr 

checkLhs gs ls (AssToPair (PairSnd expr))
  | isRight z = z
  | otherwise =  case x of
          PairType (PairInstance _ t2) -> Left (Just (unPairElemType t2))
          _                            ->  Right ("Semantic Error: " 
                                           ++ show z ++ " is Not A Pair")
    where
      Left (Just x) = z
      z = checkExpr gs ls expr 

unPairElemType :: PairElemType -> Type
unPairElemType (BasicPairElem t) = BaseType t
unPairElemType (ArrayPairElem t) = ArrayType t
unPairElemType PairPairElem      = AnyType

checkExpr    :: SymbolTable -> SymbolTable -> Expr -> CheckType
checkExpr gs ls (IntLiteral i) 
  = Left (Just (BaseType IntType))
checkExpr gs ls (BoolLiteral b)
  = Left (Just (BaseType BoolType))
checkExpr gs ls (CharLiteral c)
  = Left (Just (BaseType CharType))
checkExpr gs ls (StringLiteral s)
  = Left (Just (BaseType StringType))
checkExpr gs ls (PairLiteral p)
  = Left (Just (PairType NullPair))
  
checkExpr gs ls (IdentExpr i)
  | isNothing x && isNothing y && lhsStructExistance
                                = Right ("Semantic error: the Identifier " 
                                            ++ show i ++
                                            " does not exist in this scope")
  | isNothing x                 = Left y
  | otherwise                   = Left x
    where 
      x   = lookup i ls
      y   = lookup i gs

      lhsStructExistance = null list1 && null list2 
      list1 = [e | e@(StructBlock x _, _) <- ls , x == i]
      list2 = [e | e@(StructBlock x _, _) <- gs , x == i]

checkExpr gs ls (ArrayElemExpr (ArrayElem i exprs))
  | isNothing localType && isNothing globalType
                                         = Right ("The Identifier"
                                           ++ show i ++
                                           " does not exist in this scope")
  | isRight errorsCheckTypes    = errorsCheckTypes
  | notInts /= []               = Right "The Array Index is not an Integer"
  | isNothing localType         = stripper noIndices (fromJust globalType)
  | otherwise                   = stripper noIndices (fromJust localType)
    where
      localType = lookup i ls
      globalType = lookup i gs 
      notInts = filter (\p -> p /= Left (Just (BaseType IntType))) checktypes'
      errorsCheckTypes = foldr1 combineErrors checktypes'
      checktypes' = map (checkExpr gs ls) exprs

      noIndices = length exprs

      stripper :: Int -> Type -> CheckType
      stripper 0 t = Left (Just t)
      stripper n (BaseType StringType) = Left (Just (BaseType CharType))
      stripper n (ArrayType(ArrayInstance t)) = stripper (n-1) t
      stripper _ _ = Right "Semantic Error: Dimensional Mismatch"

checkExpr gs ls (UnOpExpr op exp)
  | isRight z                   = z
  | isNothing retType     = Right ("Semantic error: the Operation " 
                                   ++ show op ++ " cannot be applied to type " 
                                   ++ show t)
  | otherwise                   = Left (Just rType)
    where 
      z = checkExpr gs ls exp
      Left (Just t) = z
      retType = checkOp op t
      (Just rType) = retType
      
checkExpr gs ls (BinOpExpr op exp1 exp2)
  | isRight z1 || isRight z2    = combineErrors z1 z2
  | t == BaseType IntCharType   = if (z1 == Left (Just (BaseType IntType)) || 
                                      z1 == Left (Just (BaseType CharType)))
                                  && (z2 == Left (Just (BaseType IntType)) ||
                                      z2 == Left (Just (BaseType CharType)))
                                  then Left (Just ret)
                                   else Right ("The Operation checked against" 
                                           ++" IntCharType " ++ show op 
                                           ++ " cannot be applied to type " 
                                           ++ show t1 ++ ", it requires " 
                                           ++ show t)
  | t1 /= t                     = Right ("Semantic error: the Operation "
                                           ++ show op 
                                           ++ " cannot be applied to type " 
                                           ++ show t1 ++ ", it requires " 
                                           ++ show t)
  | t2 /= t                     = Right ("Semantic error: the Operation "
                                           ++ show op 
                                           ++ " cannot be applied to type " 
                                           ++ show t2 ++ ", it requires " 
                                           ++ show t)
  | otherwise                   = Left (Just ret)
    where 
      (t, ret)  = checkBinOp op
      z1 = checkExpr gs ls exp1
      z2 = checkExpr gs ls exp2
      Left (Just t1) = z1
      Left (Just t2) = z2

checkExpr gs ls (BracketedExpr exp)
   = checkExpr gs ls exp



checkOp :: UnOp -> Type -> Maybe Type
checkOp FactOp t 
  | t == BaseType BoolType  = Just (BaseType BoolType)
  | otherwise                = Nothing

checkOp NegOp t  
  | t == BaseType IntType   = Just (BaseType IntType)
  | otherwise               = Nothing

checkOp LenOp (ArrayType _) = Just (BaseType IntType)
checkOp LenOp (BaseType StringType)
                            = Just (BaseType IntType)
checkOp LenOp _             = Nothing

checkOp OrdOp t 
  | t == BaseType CharType  = Just (BaseType IntType)
  | otherwise               = Nothing

checkOp ChrOp t
  | t == BaseType IntType   = Just (BaseType CharType)
  | otherwise               = Nothing

checkBinOp :: BinOp -> (Type, Type)
checkBinOp AndOp = (BaseType BoolType, BaseType BoolType)
checkBinOp OrOp  = (BaseType BoolType, BaseType BoolType)
checkBinOp EqOp  = (AnyType, BaseType BoolType)
checkBinOp NEqOp = (AnyType, BaseType BoolType)
checkBinOp GTOp  = (BaseType IntCharType, BaseType BoolType)
checkBinOp GTEqOp = (BaseType IntCharType, BaseType BoolType)
checkBinOp LTOp  = (BaseType IntCharType, BaseType BoolType)
checkBinOp LTEqOp = (BaseType IntCharType, BaseType BoolType)
checkBinOp _     = (BaseType IntType, BaseType IntType)
