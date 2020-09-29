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
\type 'q', 'quit', or 'exit' to quit.\n\
\type 'h' or 'help' for help.\n\
\\n\
\This program will roll any of the standard D&D dice for you\n\
\(i.e. d4, d6, d8, d10, d12, d20, or d100) and lets you roll\n\
\multiple dice at once, e.g.: 3d6\n\
\\n\
\You can also roll multiple dice of different types at once:\n\
\\n\
\    3d4 + 2d6\n\
\\n\
\It also lets you add numeric constants (e.g. 7) and multiply\n\
\numeric constants together (7 * 3) to modify your roll.\n\
\\n\
\Say you rolled three attacks which each gave 2d8 + 7 damage;\n\
\you would type the damage in like so:\n\
\\n\
\    6d8 + 7 * 3\n\
\\n"

-- U+1F3B2 🎲
prompt = "{🎲}> "

quitInputs = ["quit", "q", "exit"]
helpInputs = ["help", "h"]

run :: InputT IO ()
run = do
  line <- unpack . strip . pack . fromMaybe "" <$> getInputLine prompt
  if | elem line quitInputs -> return ()
     | elem line helpInputs -> liftIO showHelp >> run
     | null line            -> run
     | otherwise            -> process line >> run

process :: String -> InputT IO ()
process line = do
  let parsed = parseLine "input" line
  case parsed of
    Left e -> do
      liftIO (print e)
      outputStrLn ("type one of the following for help: " ++ show helpInputs)
    Right p -> do
      (a, w) <- evalLine p
      outputStrLn ("total: " ++ show a)
      outputStrLn ("breakdown:  " ++ w)
