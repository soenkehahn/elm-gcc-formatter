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

format :: Maybe FilePath -> String -> Either String String
format mParent input = case parse input of
  Right messages -> formatMessages
    $ map (addParent mParent) messages
  Left err -> Left err

parse :: String -> Either String [ElmMessage]
parse input =
  eitherDecode' (cs input) <|>
  eitherDecode' (cs (stripLastLine input))

stripLastLine :: String -> String
stripLastLine =
  lines >>> init >>> unlines >>> (++ "\n")

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

formatMessages messages =
  Right $ intercalate "\n\n" $ map formatMessage messages

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
