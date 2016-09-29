module MarkLex where

import Data.Maybe
import Data.Text (split, pack, unpack)
import Data.Char

data ProgramLine = ProgramLine String
                 | BeginCond RunCond
                 | EndCond
                 | ImportStatement String
                 | Def String String
                 deriving Show

data RunCond = IfDef String
             | IfnDef String
             deriving Show

lexProgram :: String -> [ProgramLine]
lexProgram program
  = lexLines (endBy '\n' program)

lexLines :: [String] -> [ProgramLine]
lexLines 
  = concatMap lexLine

lexLine :: String -> [ProgramLine]
lexLine []                    = []
lexLine line
  | startswith "@IFDEF"  cLine = [BeginCond (IfDef (splitLine !! 1))]
  | startswith "@IFNDEF" cLine = [BeginCond (IfnDef (splitLine !! 1))]
  | startswith "@ENDIF"  cLine = [EndCond]
  | startswith "@DEFINE" cLine = [Def (splitLine !! 1) (splitLine !! 2)]
  | startswith "@IMPORT" cLine = [ImportStatement ((concat . tail) splitLine)]
  | null lineNoComments        = []
  | otherwise                  = [ProgramLine lineNoComments]
  where
      lineNoComments = filterComments line False
      cLine = dropWhile isSpace line
      splitLine = endBy ' ' line 

startswith :: String -> String -> Bool
startswith s1 s2
  = and (zipWith (==) s1 s2) && (length s1 <= length s2)

endBy:: Char -> String -> [String]
endBy splitOn 
  = removeNull . map unpack . split (==splitOn) . pack

removeNull :: [[a]] -> [[a]]
removeNull
  = filter (not . null)

filterComments :: String -> Bool -> String
filterComments [] _     = []
filterComments ('\'' : '#' : '\'' : cs) isStr
  = "\'#\'" ++ filterComments cs isStr
filterComments ('\\' : '\"' : cs) isStr
  = "\\\"" ++ filterComments cs isStr
filterComments (c:cs) isStr
  | isStr && c == '\"'  = '\"' : filterComments cs False
  | isStr               = c : filterComments cs True
  | c == '#'            = []
  | c == '\"'           = '\"' : filterComments cs True
  | otherwise           = c : filterComments cs isStr
