{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Format where

import GHC.Generics
import Control.Applicative
import Control.Arrow
import Data.List
import System.Exit
import Data.Aeson
import Data.String.Conversions
import System.FilePath

format :: Maybe FilePath -> String -> (String, ExitCode)
format mParent input =
  first (intercalate "\n" >>> (++ "\n")) $
  foldl' inner ([], ExitSuccess) (map parse (lines input))
  where
    inner acc parsed = case parsed of
      Right messages ->
        let outMessages = formatMessages
              $ map (addParent mParent) messages
            exitCode = if null messages
              then snd acc
              else ExitFailure 1
        in (fst acc ++ [outMessages], exitCode)
      Left err -> (fst acc ++ [err], snd acc)

parse :: String -> Either String [ElmMessage]
parse input =
  case eitherDecode' (cs input) of
    Left _ -> Left input
    Right x -> Right x

data ElmMessage
  = ElmMessage {
    file :: String,
    region :: Region,
    tag :: String,
    overview :: String,
    details :: String
  }
  deriving (Generic)

instance FromJSON ElmMessage

addParent :: Maybe FilePath -> ElmMessage -> ElmMessage
addParent mParent message = case mParent of
  Just parent -> message{
    file =
      ("." </>) $
      normalise $
        parent </> file message
  }
  Nothing -> message

formatMessages :: [ElmMessage] -> String
formatMessages messages =
  intercalate "\n\n" $ map formatMessage messages

formatMessage :: ElmMessage -> String
formatMessage message =
  "\n" ++
  file message ++ ":" ++
  show (line (start (region message))) ++ ":" ++
  show (column (start (region message))) ++ ": " ++
  tag message ++ "\n" ++
  "\n" ++
  "  " ++ overview message ++ "\n" ++
  "\n" ++
  "  " ++ details message

data Region
  = Region {
    start :: Range
  }
  deriving (Generic)
instance FromJSON Region

data Range
  = Range {
    line :: Int,
    column :: Int
  }
  deriving (Generic)
instance FromJSON Range
