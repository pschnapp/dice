{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (pack, strip, unpack)
import System.Console.Haskeline
import System.Environment
import System.IO
import System.Random

import Lib


instance MakesGen (InputT IO) where
  makeGen = liftIO newStdGen

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["help"] -> showHelp
    ["h"]    -> showHelp
    [] -> runInputT defaultSettings run
    _  -> runInputT defaultSettings $ process (unwords args)

showHelp :: IO ()
showHelp = putStrLn "\
\\n\
\type 'q', 'quit', 'e', or 'exit' to quit.\n\
\type 'h' or 'help' for help.\n\
\\n\
\This program is a calculator for rolling dice. It performs the\n\
\following operations:\n\
\\n\
\     +  addition  (binary)\n\
\     -  subtraction/negation  (binary/unary)\n\
\     *  multiplication  (binary)\n\
\     d  rolling a die  (binary/unary)\n\
\    >d  rolling a die with advantage  (binary/unary)\n\
\    <d  rolling a die with disadvantage  (binary/unary)\n\
\     x  run a roll expression multiple times  (binary)\n\
\\n\
\Parentheses can be used to denote sub-expressions.\n\
\\n\
\For example:\n\
\\n\
\    To roll for an attack with advantage and inspiration\n\
\    (including an ability modifier and proficiency):\n\
\\n\
\        {🎲}>  >d20 + d4 + 5\n\
\        total: 25\n\
\        breakdown:  1 >d 20 (Σ[(19>11)]=19) + 1 d 4 (Σ[1]=1) + 5\n\
\\n\
\    Then to roll for damage if the attack succeeds:\n\
\\n\
\        {🎲}>  2d6 + 3\n\
\        total: 11\n\
\        breakdown:  2 d 6 (Σ[4,4]=8) + 3\n\
\\n\
\    You can also do multiple attack rolls at once using the `x` operator:\n\
\\n\
\        {🎲}>  2 x d20+4\n\
\        total: 25\n\
\        breakdown:  {(2 x): [1 d 20 (Σ[14]=14) + 4]=18 + [1 d 20 (Σ[3]=3) + 4]=7}=25\n\
\\n"

-- U+1F3B2 🎲
prompt = "{🎲}>  "

quitInputs = ["quit", "q", "exit", "e"]
helpInputs = ["help", "h"]

run :: InputT IO ()
run = do
  line <- unpack . strip . pack . fromMaybe "" <$> getInputLine prompt
  if | elem line quitInputs -> return ()
     | elem line helpInputs -> liftIO showHelp >> run
     | null line            -> run
     | otherwise            -> process line >> run

process :: String -> InputT IO ()
process line =
  case parseLine "input" line of
    Left e -> do
      outputStrLn e
      outputStrLn ("type one of the following for help: " ++ show helpInputs)
    Right p -> do
      (a, w) <- evalLine p
      outputStrLn ("total: " ++ show a)
      outputStrLn ("breakdown:  " ++ w)
