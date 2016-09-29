module Main(main) where

import Grammar
import Asm
import Parser ( parseExp )
import Analyser
import Translate
import Mark

import System.Environment ( getArgs )
import Data.Text
import Data.Maybe
import System.Exit
import Control.DeepSeq

main :: IO ()
main = do
  args <- getArgs
  parseLoc <- case args of
    []  -> return "<stdin>"
    [f] -> return f
    _   -> error "expected max. 1 argument"

  parseTarget <- case parseLoc of
    "<stdin>"   -> getContents
    _           -> readFile parseLoc

  parseTarget <- return (preprocess parseTarget)
  parseResult <- fmap (parseExp parseLoc) parseTarget
  
  (strOut, retCode) <- return (returnCode parseResult)
  outputLoc <- return (outputFileName parseLoc)
  _ <- writeFile outputLoc strOut
  putStrLn strOut
  case retCode of
    0 -> exitSuccess
    _ -> exitWith (ExitFailure retCode)

pr         :: [String] -> (String, IO String)
pr []      = ("<stdin", getContents)
pr [f]     = (f , readFile f)
pr _       = error ""

outputFileName :: String -> String
outputFileName strOut
  | isNothing fLoc      = "a.s"
  | otherwise           = unpack fName ++ ".s"
  where
      fLoc  = stripSuffix (pack ".wacc") (pack strOut)
      fName = Prelude.last (splitOn (pack "/") (fromJust fLoc))


returnCode              :: Either String Program -> (String, Int)
returnCode (Left err)     = (err, 100)
returnCode (Right p)
  | isRight semanticRes   = (semanticMsg, 200)
--  | otherwise             = (show t , 0)
--  | otherwise             = (show t ++ "\n\n" ++ show (AsmProgram is), 0)
  | otherwise             = (show is, 0)
    where
      (semanticRes, t)  = checkProgram p
      semanticFail      = isRight semanticRes
      (Right semanticMsg)  
                        = semanticRes
      is                = translateAndOptimise t
