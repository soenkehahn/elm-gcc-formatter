
module Main where

import Format
import System.Exit

main :: IO ()
main = do
  input <- getContents
  case format input of
    Left err -> die err
    Right output -> putStr output
