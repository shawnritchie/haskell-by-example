module Main where

--import Lib
--import CH04.SplitLines
--
--main :: IO ()
--main = someFunc

import System.Environment (getArgs)
import CH04.SplitLines
import CH04.Exercises
import Control.Exception
import Data.Char
import Data.List

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction input = unlines $ map (head . splitWith (==' ')) (splitLines input)