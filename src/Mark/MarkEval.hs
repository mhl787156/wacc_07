module Mark where

import MarkLex
import Data.Text (splitOn, pack, unpack)
import Data.List
import Data.Maybe
import System.Info
import Control.Monad

preprocess :: String -> IO String
preprocess 
  = (>>= recoverProgram) . (\p->processLines p initialSymbols []) . lexProgram

initialSymbols :: [(String, String)]
initialSymbols =  [("OS_NAME"   , System.Info.os),
                   ("ARCH_NAME" , System.Info.arch)]

recoverProgram :: [ProgramLine] -> IO String
recoverProgram ls
  | null ls         = return []
  | otherwise       = liftM (foldl1 ((++) . (++"\n"))) plStrings
  where
      plStrings     = liftM (map (\pl -> case pl of ProgramLine pl -> pl
                                                    _              -> "")) (return ls)

processLines :: [ProgramLine] -> [(String, String)] -> [Bool] -> IO [ProgramLine]
processLines [] _ _  = return []
processLines (p:ps) symTable showing
  = case p of
        ProgramLine pl      -> plVal
        BeginCond c         -> processLines ps symTable (processCond c symTable : showing)
        EndCond             -> processLines ps symTable (tail showing)
        ImportStatement tgt -> imVal tgt
        Def id val          -> processLines ps (nub ((id, val) : symTable)) showing
    where
        plVal = if null showing || head showing 
                    then liftM (processPl p symTable :) (processLines ps symTable showing)
                    else processLines ps symTable showing
        imVal tgt = if null showing || head showing 
                    then liftM2 (++) (processImport tgt symTable showing) 
                                     (processLines ps symTable showing)
                    else processLines ps symTable showing

processImport :: String -> [(String, String)] -> [Bool] -> IO [ProgramLine]
processImport tgt symTable showing
  = do
        fileContents <- readFile tgt
        processedFile <- ((\p->processLines p symTable showing) . lexProgram) fileContents
        return processedFile

processPl :: ProgramLine -> [(String, String)] -> ProgramLine
processPl (ProgramLine p) (def:symTable)
  = processPl (ProgramLine (replace p def)) symTable
processPl p []
  = p

replace :: String -> (String, String) -> String
replace text (old, new) 
  = foldl1 ((++) . (++new)) (map unpack (splitOn (pack old) (pack text)))

processCond :: RunCond -> [(String, String)] -> Bool
processCond (IfnDef c) symTable
  = isNothing (lookup c symTable)
processCond (IfDef c) symTable
  = not (processCond (IfnDef c) symTable)
