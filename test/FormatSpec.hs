
module FormatSpec where

import Test.Hspec
import System.Exit

import Format

spec :: Spec
spec = do
  describe "format" $ do
    let expected = unlines $
          "./src/Levels.elm:20:12: NAMING ERROR" :
          "  Cannot find variable `Dict.fromLst`." :
          "" :
          "  `Dict` does not expose `fromLst`. Maybe you want one of the following?\n\n    Dict.fromList" :
          []
    it "converts elm's json output to gcc format" $ do
      errorText <- readFile "test/elm-error.txt"
      let Right (output, _) = format Nothing errorText
      output `shouldBe` expected

    it "tries to parse the output without the last line" $ do
      file <- readFile "test/elm-error.txt"
      let errorText = file ++ "\nsuccessful compilation, bla bla\n"
      let Right (output, _) = format Nothing errorText
      output `shouldBe` expected

    it "allows to specify a parent directory for found files" $ do
      errorText <- readFile "test/elm-error.txt"
      let Right (output, _) = format (Just "parent") errorText
      head (lines output) `shouldBe`
        "./parent/src/Levels.elm:20:12: NAMING ERROR"

    describe "exit codes" $ do
      it "returns a zero exit code if there's no errors" $ do
        let errorText = "[]"
        let Right (output, exitCode) =
              format (Just "parent") errorText
        exitCode `shouldBe` ExitSuccess

      it "returns with a non-zero exit code if there's errors" $ do
        errorText <- readFile "test/elm-error.txt"
        let Right (output, exitCode) = format (Just "parent") errorText
        exitCode `shouldBe` ExitFailure 1
