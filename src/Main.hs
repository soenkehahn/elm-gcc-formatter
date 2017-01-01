{-# LANGUAGE DeriveGeneric #-}

module Main where

import Format
import System.Exit
import WithCli

data Options
  = Options {
    parentDir :: Maybe FilePath
  }
  deriving (Generic)

instance HasArguments Options

main :: IO ()
main = withCli $ \ options -> do
  input <- getContents
  case format (parentDir options) input of
    Left err -> die err
    Right (output, exitCode) -> do
      putStr output
      exitWith exitCode
      return ()
