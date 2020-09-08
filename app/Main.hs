{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Data.List
import System.IO
import System.Random

import Lib


main :: IO ()
main = run

run = do
  putStr prompt
  hFlush stdout
  line <- getLine
  if | line == "quit" || line == "q" -> return ()
     | line == "help" || line == "h" -> showHelp     >> run
     | otherwise                     -> process line >> run

-- U+1F3B2
prompt = "🎲>  "

showHelp = putStrLn "todo..."

process :: String -> IO ()
process line = do
  let parsed = parseLine "input" line
  case parsed of
    Left e -> print e
    Right p -> do
      (a, w) <- evalLine p
      putStrLn ("total: " ++ show a)
      putStrLn ("breakdown: " ++ w)

instance MakesGen IO where
  makeGen = newStdGen
