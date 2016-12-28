{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Format where

import GHC.Generics
import Data.List
import System.Exit
import Data.Aeson
import Data.String.Conversions

format :: String -> Either String String
format input = case eitherDecode' (cs input) of
  Right (messages :: [ElmMessage]) -> Right $ intercalate "\n\n" $ map formatMessage messages
  Left err -> Left err

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

formatMessage :: ElmMessage -> String
formatMessage message =
  file message ++ ":" ++
  show (line (start (region message))) ++ ":" ++
  show (column (start (region message))) ++ ": " ++
  tag message ++ "\n" ++
  "  " ++ overview message ++ "\n" ++
  "\n" ++
  "  " ++ details message ++ "\n"

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
