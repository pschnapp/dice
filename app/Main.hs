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
prompt = "🎲> "

showHelp = putStrLn "todo..."

process :: String -> IO ()
process line = do
  let parsed = parseLine "input" line
  case parsed of
    Left e -> putStrLn (show e)
    Right p -> do
      (ns,ss) <- unzip <$> mapM compute p
      putStrLn ("total: " ++ (show $ sum ns))
      putStrLn ("breakdown: " ++ intercalate "  +  " ss)

compute (Die n d) = do
  gen <- newStdGen
  let rs = take n $ rolls d gen
  let str = concat [show n, show d, " (", intercalate ", " (map show rs), ")"]
  return (sum rs, str)
compute (Number n) = return (n, show n)
